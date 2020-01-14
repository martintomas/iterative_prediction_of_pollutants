'''
Created on Oct 30, 2014

@author: trn
'''

import os
import numpy as np
import traceback
import time
from learning import *
from learningR import RLearning
from learningData import DataFactory
from learningCore import LearningFactory

scratchdir = os.environ['SCRATCHDIR']
saveScratch = os.environ['SAVESCRATCH']
iteration = int(os.environ['ITERATION'])
cores = int(os.environ['CORES'])-1
testing = int(os.environ['TESTING']) #1 == run tests, 0 == don't do testing
initDir = os.environ['INITDIR']
learningDB = os.environ['LEARNINGDB']
typeLearning = os.environ['TYPELEARNING'] #can be cell or region

ignoredTags = ('I','NA','R')
howOftenSaveOutputs = 100 #after how many time always save (in seconds)
maxRowsPerProcess = 100
        
class LearningProcess (RowProcess):
    def __init__(self,learningCore,learningR, mapRows, rows, cols, dbAddr,inData,outData,inTestData=None,outTestData=None, info=None):
        RowProcess.__init__(self,learningCore=learningCore,learningR=learningR,rows=rows, cols=cols, dbAddr=dbAddr,inData=inData,outData=outData, ignoredTags=ignoredTags, inTestData=inTestData,outTestData=outTestData)
        self.mapRows = mapRows
        self.info = info
        
#    def initValues(self):
#        #init values
#        for i in range(len(self.rows)):
#            self.outData.initValues(self.cols[i,],i)
#            self.outTestData.initValues(self.cols[i,],i)          
        
    def run(self):
        #print("Starting thread for row %i" % self.row)
        #print('Creating samples')
        row = ",".join("%s" % (t+1) for t in self.rows if self.mapRows[t]!=2)
        #call("Rscript %s/create-sample.r %s %s/pom-data.db %s sample %s %s %s %s %s" % (scriptDir,self.dbAddr,scratchdir,scratchdir,self.row+1,col,typeSelect,method1,method2), shell=True, stdout=devnull, stderr=devnull)
        #call("Rscript %s/create-sample2.r %s %s/pom-data.db %s/map.csv %s sample %s %s %s %s" % (scriptDir,self.dbAddr,scratchdir,scratchdir,scratchdir,row,typeSelect,method1,method2), shell=True, stdout=devnull, stderr=devnull)        
        
        if row: #some row exists
            start = time.time()        
            learningR.samples(row)  
            self.info.incTime(start,time.time())
        
        sampleAddr = "%s/sample" % (scratchdir)
        
        for i in self.rows:
            try:            
                if self.mapRows[i] != 2: #avoid ignored rows
            
                    start = time.time()
                    
                    self.runLearning(sampleAddr,self.cols[i,],i)        

                    self.removeSamples(sampleAddr,self.cols[i,])
                    
                    self.info.incFinished()
                    self.info.incTime(start,time.time())
            except Exception:
                print("Error occured in row %i" % i)
                print traceback.format_exc()
                pass

def runComputationParallel(dbAddr,learningR,myData,mapOfRows):
    
    dataFactory = DataFactory(dbAddr) #used to load data and appropriate classes
    
    #prepare input/output data
    [inData,outData] = dataFactory.getData(typeLearning,data=myData,scratchdir=scratchdir)
    
    #prepare data for testing if needed
    if testing:         
        [inTestData,outTestData] = dataFactory.getDataTest(typeLearning,inData,outData,scratchdir=scratchdir)
    else:
        [inTestData,outTestData] = [None,None]
        
    #prepare info object
    info = dataFactory.getInfo(typeLearning,cores)
    
    splitRows = splitRowsToGroups(mapOfRows,cores) #groups of rows for each process 

    workingProcesses = [0 for x in splitRows] #0 for not working, 1 for working and 2 for finished
    
    #prepare processes    
    mapOfProcess = [None for x in splitRows] #pointers to processes   
    
    #prepare core of learning
    learningCore = LearningFactory().getLearningModule()
    
    for i in range(len(splitRows)):
        print("Process number %i has started"%(i))
        mapOfProcess[i] = LearningProcess(learningCore=learningCore,learningR=learningR,mapRows=mapOfRows,rows=splitRows[i],cols=myData, dbAddr=dbAddr,inData=inData,outData=outData,inTestData=inTestData,outTestData=outTestData,info=info)
        mapOfProcess[i].start()   
        workingProcesses[i] = 1
    
    lastInfoSave = time.time()
    while True:
        if( not ( (0 in workingProcesses) or (1 in workingProcesses) ) ):
            print("All procesess finished!!")
            break
        if(((lastInfoSave+howOftenSaveOutputs)<time.time())):
            outData.save(saveScratch,iteration) 
            if(info.numFinished()!=0):
                info.save(saveScratch,iteration)  
            if testing:
                outTestData.save(saveScratch,iteration)
            lastInfoSave = time.time()
        for i in range(len(workingProcesses)):        
            if workingProcesses[i] == 1:
                if not mapOfProcess[i].is_alive():
                    #for j in range(len(splitRows[i])):
                    #    output.saveRow(splitRows[i][j],mapOfProcess[i].getOutput().getData(j)) #save output
                    #    if test != None:
                    #        testOutput.saveRow(splitRows[i][j],mapOfProcess[i].getTests().getData(j))
                    mapOfProcess[i] = None
                    workingProcesses[i] = 2 #slot is free now
                    
    #save info and outputs for last time
    outData.save(saveScratch,iteration)
    info.save(saveScratch,iteration)
    if testing:
        outTestData.save(saveScratch,iteration)

def runComputationParallelLimited(dbAddr,learningR,myData,mapOfRows): 
    '''Only a few rows per process is always created'''
    
    dataFactory = DataFactory(dbAddr) #used to load data and appropriate classes
    
    #prepare input/output data
    [inData,outData] = dataFactory.getData(typeLearning,data=myData,scratchdir=scratchdir)
    
    #prepare data for testing if needed
    if testing:         
        [inTestData,outTestData] = dataFactory.getDataTest(typeLearning,inData,outData,scratchdir=scratchdir)
    else:
        [inTestData,outTestData] = [None,None]
        
    #prepare info object
    info = dataFactory.getInfo(typeLearning,cores)
    
    #splitRows = splitRowsToGroups(mapOfRows,cores) #groups of rows for each process 
    splitRows = splitRow(mapOfRows,maxRowsPerProcess,cores) #groups of rows, each with max n rows

    #workingProcesses = [0 for x in splitRows] #0 for not working, 1 for working and 2 for finished
    mapOfRowsProcesses = [0 for x in splitRows] #0 for not working, 1 for working and 2 for finished

    #prepare processes    
    mapOfProcess = [None for x in range(cores)] #pointers to processes 
    mapOfProcessRows = [None for x in range(cores)] #holds info, which process is computing which group of rows
    
    #prepare core of learning
    learningCore = LearningFactory().getLearningModule()
    
    lastInfoSave = time.time()
    while True:
        if( not ( (0 in mapOfRowsProcesses) or (1 in mapOfRowsProcesses) ) ):
            print("All procesess finished!!")
            break
        if(((lastInfoSave+howOftenSaveOutputs)<time.time())):
            outData.save(saveScratch,iteration) 
            if(info.numFinished()!=0):
                info.save(saveScratch,iteration)  
            if testing:
                outTestData.save(saveScratch,iteration)
            lastInfoSave = time.time()
            
        for i in range(cores): #check all cores
            waittingRow = 0 in mapOfRowsProcesses
            if mapOfProcess[i] is None: #thread is waitting
                if waittingRow: #some rows are waitting
                    indexRow = mapOfRowsProcesses.index(0)
                    mapOfProcess[i] = LearningProcess(learningCore=learningCore,learningR=learningR,mapRows=mapOfRows,rows=splitRows[indexRow],cols=myData, dbAddr=dbAddr,inData=inData,outData=outData,inTestData=inTestData,outTestData=outTestData,info=info)
                    mapOfProcess[i].start()
                    mapOfRowsProcesses[indexRow] = 1
                    mapOfProcessRows[i] = indexRow
            else: #thread is occupying thread slot
                if not mapOfProcess[i].is_alive():
                    indexRow = mapOfProcessRows[i]
                    mapOfRowsProcesses[indexRow] = 2 #group of rows is now finished
                    mapOfProcess[i] = None #slot is free now
                    #output.saveRow(indexRow,mapOfProcess[i].getOutput().getData()) #save output
                    #if test != None:
                    #    testOutput.saveRow(indexRow,mapOfProcess[i].getTests().getData())
                    #info.incFinished() #one more is finished
                    #info.incTime(mapOfProcess[i].getTimeF(),time.time()) #save running time
                    #saveInfo = False                        
                    
    #save info and outputs for last time
    outData.save(saveScratch,iteration)
    info.save(saveScratch,iteration)
    if testing:
        outTestData.save(saveScratch,iteration) 

if __name__ == '__main__':
    
    dbAddr = "%s/%s"%(initDir,learningDB)
    learningR = RLearning(scratchdir,saveScratch,iteration,cores,initDir,learningDB,testing)    
    
    if(not learningR.checkTypeLearning()):
        print('Unknown learning method selected!!!')
        exit()    
    
    learningR.preProcess()
    
    #test if map was created
    predMap = ("%s/map.csv" % (scratchdir))
    if(not os.path.exists(predMap)):
        print('Prediction map was not find!!!')
        exit()
    
    #load map.csv
    myData = np.genfromtxt(predMap, dtype='S14' ,delimiter=',',autostrip=True, skip_header=1)
    if len(myData.shape) == 1: #I want matrix, is only array
        myData = myData.reshape(myData.shape[0],1) #create matrix
    #mapOfRows #0 will be predicted, 2 row is ignored
    [myData,mapOfRows] = updateData(myData,ignoredTags)
#    mapOfRows = mapOfRows[0:10]   #just for testing      
  
    #runComputationParallel(dbAddr,learningR,myData,mapOfRows)
    runComputationParallelLimited(dbAddr,learningR,myData,mapOfRows)    
    
    learningR.postProcess()
    
    exit()
