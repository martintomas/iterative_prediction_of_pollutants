# -*- coding: utf-8 -*-
"""
Created on Mon Nov  3 12:59:49 2014

@author: trn
"""

import numpy as np
from multiprocessing import Process
import os
import traceback

#remove unwanted things from numpy array
def removeStrArray(data,string='\"'):
    return([w.replace(string, '') for w in data])

#check row if there are some interesting cols
def checkRow(row,ignoredTags):
    for i in row:
        if(i not in ignoredTags):
            return(True)
    return(False)
    
#data - map.csv numpy data
#returns updated data + row list (which rows ignore)
def updateData(data,ignoredTags):
    dataNew = data
    rows = [0 for x in range(data.shape[0])]
    for i in range(data.shape[0]):
        dataNew[i,] = removeStrArray(data[i,])
        if(not checkRow(dataNew[i,],ignoredTags)):
            rows[i] = 2 #ignore this row, there are not important values
    return([dataNew,rows])

def splitRowsToGroups(rows,cores):
    '''Split array to different arrays, number of arrays is defined'''
    if(cores>len(rows)):
        num = len(rows)
    else:
        num = cores
    splitRows = range(len(rows))
    return([np.array(splitRows[i::num],dtype='i') for i in range(num)]) 

def splitRowsToGroupsLimited(rows,maxRows):
    '''Split rows to different arrays, number of rows in one cell is defined'''
    if (len(rows)%maxRows)==0:
        num = (len(rows)/maxRows)
    else:
        num = (len(rows)/maxRows)+1
    splitRows = np.array(range(len(rows)))
    return([splitRows[i*maxRows:(i+1)*maxRows] for i in range(num)])

def splitRow(rows,maxRows,cores):
    if len(rows) > (maxRows*cores):
        return(splitRowsToGroupsLimited(rows,maxRows))
    else:
        return(splitRowsToGroups(rows,cores))
    

class Info:
    def __init__(self,cores,end=''):
        self.timeF = 0
        self.finished = 0
        self.cores = cores
        self.end=end
    def save(self,addr,iteration):
        with open("%s/iteration-%i-info%s.txt"%(addr,iteration,self.end), "w") as infoFile:
            infoFile.write("Sum of run time: %f\n" % (self.timeF))    
            infoFile.write("Run time per col: %f\n" % (self.timeF/self.finished))
            infoFile.write("Run time per cores: %f\n" % (self.timeF/self.cores))
            infoFile.write("Run time per col and core: %f\n" % ((self.timeF/self.finished)/self.cores))
            infoFile.write("Finished rows: %i" % (self.finished))
    def incFinished(self):
        self.finished += 1
    def incTime(self,fromT,toT):
        self.timeF += (toT-fromT)
    def numFinished(self):
        return(self.finished)
        
class RowProcess (Process):
    def __init__(self, learningCore, learningR, rows, cols, dbAddr, inData,outData,ignoredTags, inTestData=None,outTestData=None):
        Process.__init__(self)
        self.learningCore=learningCore
        self.rows = rows
        self.cols = cols
        self.outData = outData
        self.dbAddr = dbAddr
        self.inData = inData
        self.inTestData = inTestData
        self.outTestData = outTestData
        self.ignoredTags = ignoredTags
        self.learningR = learningR
        
        #self.initValues()
    
    def runLearning(self,addr,cols,row):
        for i in range(len(cols)):
            try:
                if cols[i] not in self.ignoredTags:
                    fileSample = "%s_%s.data"%(addr,cols[i])

                    [num_input,num_output] = self.annDim(fileSample)
                
                    learningAdapter = self.learningCore.construct(num_input,num_output)
                    self.learningCore.train(learningAdapter,fileSample)

                    self.inData.predict(self.learningCore,learningAdapter,self.outData,row,i,num=cols[i],addr=addr)
                    if self.inTestData != None:
                        self.inTestData.predict(self.learningCore,learningAdapter,self.outTestData,row,i,num=cols[i],addr=addr)
                        
            except Exception:
                print("Exception during prediction of sample %s occured, using NA value" % cols[i])
                print traceback.format_exc()
                self.outData.saveError('NA',row,i)
                if self.outTestData != None:
                    self.outTestData.saveError('NA',row,i)
                pass
            
    def annDim(self,addr):
        with open(addr, 'r') as f:
            firstLine = f.readline()
            x = str.split(firstLine,' ') # load used cells
        return([int(x[1]),int(x[2])])
           
    def removeSamples(self,addr,cols):
        for i in cols:
            if i not in self.ignoredTags: #file was generated for this col
                try:
                    os.remove("%s_%s.data"%(addr,i))
                    os.remove("%s-info_%s"%(addr,i))
                except Exception:
                    #no stress, some file is missing
                    pass