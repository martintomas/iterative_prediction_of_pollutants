# -*- coding: utf-8 -*-
"""
Created on Mon Nov 10 10:03:45 2014

@author: trn
"""

import numpy as np
import sqlite3
from copy import deepcopy
from multiprocessing import managers
from learning import Info
import os

class MyManager(managers.BaseManager):
    pass # Pass is really enough. Nothing needs to be done here

class DataFactory:
    def __init__(self,db):
        self.dataLoader = DataLoader(db)
        
        #prepare manager (used for testing and output object access) #needed for multiprocess access
        MyManager.register("Output", Output)
        MyManager.register("Info", Info)
        MyManager.register("OutMeasurements", OutMeasurements)
        MyManager.register("OutBack", OutBack)
        MyManager.register("OutTest", OutTest)
        self.manager = MyManager()
        self.manager.start() 
    def getInfo(self,typeLearning,cores):
        if typeLearning in ('cell','region','regions'):
            return(self.manager.Info(cores))
        elif typeLearning == 'back': 
            return(self.manager.Info(cores,end='-back'))
        elif typeLearning in ('preregions-clas','preregions-reg'):
            return(self.manager.Info(cores,end='-preregions'))
    def getData(self,typeLearning,data=None,scratchdir=None):
        print 'Creating object for data prediction'
        inpData=None
        outData = None
        if typeLearning in ('cell','region','preregions-reg','regions'):
            inpData = InMeasurements(self.dataLoader)
            outData = self.manager.OutMeasurements(data)
        elif typeLearning == 'back':        
            pomDb = "%s/pom-data-back.db" % (scratchdir)
            keepDataInRam = int(os.environ['KEEPDATAINRAM'])
            rNum = int(os.environ['RNUM'])
            arguments = {'pomDb':pomDb,'keepDataInRam':keepDataInRam,'rNum':rNum}
            inpData = InBack(self.dataLoader,arguments=arguments)
            outData = self.manager.OutBack(data,self.dataLoader,pomDb)
        elif typeLearning == 'preregions-clas':
            inpData = InPreRegions(self.dataLoader)
            outData = self.manager.OutMeasurements(data,end='-preregions')
        else:
            raise Exception('Unknown type of learning method!!')
        return([inpData,outData])
    def getDataTest(self,typeLearning,inClass,outClass,scratchdir=None):
        print 'Creating object for testing of prediction'      
        if typeLearning in ('cell','region','preregions-clas','preregions-reg','regions'):
             inpData = InTest(inClass,self.dataLoader)             
        elif typeLearning == 'back':        
            pomDb = "%s/pom-data-back.db" % (scratchdir)
            keepDataInRam = int(os.environ['KEEPDATAINRAM'])
            rNum = int(os.environ['RNUM'])
            arguments = {'pomDb':pomDb,'keepDataInRam':keepDataInRam,'rNum':rNum}
            inpData = InTest(inClass,self.dataLoader,arguments=arguments)
        outData = self.manager.OutTest(outClass,self.dataLoader)
        return([inpData,outData])

class DataLoader:
    def __init__(self,db):
        self.db = db
        self.normalizationDataOutput = None
        self.normalizationDataInput = None                      
    def openDb(self,dbAddr=None):
        if dbAddr==None:
            return(sqlite3.connect(self.db))
        else:
            return(sqlite3.connect(dbAddr))
    def closeDb(self,db):
        db.close()
    def loadData(self,selectQuery,dbAddr=None,variables=None):
        #try:
            db = self.openDb(dbAddr=dbAddr)   
            cursor = db.cursor()
            if variables == None:
                cursor.execute(selectQuery)
            else:
                cursor.execute(selectQuery,variables)
            data = cursor.fetchall()
            self.closeDb(db)
            return(data) 
        #except Exception:
        #    raise Exception('Error during database reading!!')            
    def getNormDataOutput(self):
        if self.normalizationDataOutput == None:
            self.normalizationDataOutput = np.array(self.loadData('''SELECT min,max FROM measurement order by id'''), dtype='f')
        return(self.normalizationDataOutput)
    def getNormDataInput(self):
        if self.normalizationDataInput == None:
            self.normalizationDataInput = np.array(self.loadData('''SELECT min,max FROM inputInfo order by id'''), dtype='f')
        return(self.normalizationDataInput)

class Data:
    def getData(self):
        return(self.data)
    
class Input (Data):
    def numData(self):
        raise Exception('Implemented by children')
    def predict(self,learningCore,learningAdapter,saveClass,row,col,num=None,addr=None,test=None):
        raise Exception('Implemented by children')   

class InputMeasurements(Input):
    def loadInfo(self,addr,num):
        fileSampleInfo = "%s-info_%s"%(addr,num)
        with open(fileSampleInfo, 'r') as f:
            firstLine = f.readline()
            x = str.split(firstLine,' ') # load used cells
            x = np.array(x,dtype='i') #change to numpy array and int
        x = x - 1 #decrease indexes by 1 (R scripts use array indexeces from 1, python from 0)
        return(x)
    def normalize(self,data,normData,usedCells=None):
        try:
            if usedCells == None:
                return((data-normData[0:data.shape[0],0])/(normData[0:data.shape[0],1]-normData[0:data.shape[0],0])) #keep same shape
            else:
                return((data-normData[usedCells,0])/(normData[usedCells,1]-normData[usedCells,0])) #dimensions of data and usedCells have to be same
        except Warning:
            #probably divide by zero occured
            pass 
    
class InMeasurements (InputMeasurements):
    def __init__(self,dataLoader,test=None,arguments=None):
        if test == None:
            self.dataOrig = np.array(dataLoader.loadData('''SELECT value FROM measurement order by id'''), dtype='f').reshape(-1) #create vector
        else:
            self.dataOrig = np.array(dataLoader.loadData('''SELECT value FROM output where simulation=:sim order by measurement''',variables={"sim":str(test)}), dtype='f').reshape(-1)
        self.data = self.normalize(self.dataOrig,dataLoader.getNormDataOutput())
    def numData(self):
        return(len(self.data))
    def predict(self,learningCore,learningAdapter,saveClass,row,col,num=None,addr=None,test=None):
        usedCells = InputMeasurements.loadInfo(self,addr,num)
        #res = str(learningCore.predict(learningAdapter,self.data[usedCells])[0])
        res = "%.20f" %  (learningCore.predict(learningAdapter,self.data[usedCells])[0])      
        #res = str(ann.run(self.data[usedCells])[0])
        #print "%i %i %s"%(row,col,res)
        if test == None:
            saveClass.savePredict(res,row,col)
        else:
            saveClass.savePredict(res,row,col,test=test)

class InBack (InputMeasurements):
    def __init__(self,dataLoader,test=None,arguments=None):
        if test == None:
            test=-1
        self.keepDataInRam = arguments["keepDataInRam"]  
        self.pomDb = arguments["pomDb"]
        self.rNum = arguments["rNum"]
        #self.samples = np.array(dataLoader.loadData('''SELECT DISTINCT sample from input where simulation=:sim order by sample''',variables={"sim":str(test)},dbAddr=self.pomDb),dtype='i').reshape(-1) #create vector                
        self.samples = np.arange(self.rNum)+1 #start with 1, arange start with 0, we have to add one        
        self.data = [] #np.empty((data.shape[0],self.samples.shape[0]),dtype='f')       
        self.test = test
        if self.keepDataInRam == 1:        
            for i in self.samples:
                res = np.array(dataLoader.loadData('''SELECT value from input where simulation=:sim and sample=:sam order by id''',variables={"sim":str(test),"sam":str(i)},dbAddr=self.pomDb),dtype='f').reshape(-1) #create vector                         
                #res = np.array(dataLoader.loadData('''SELECT * from input where simulation=:sim and sample=:sam''',variables={"sim":str(test),"sam":str(i)},dbAddr=self.pomDb),dtype='f').reshape(-1)[2:] #create vector (remove first two variables (simulation and sample))    
                res = self.normalize(res,dataLoader.getNormDataInput())
                self.data.append(res) 
        else:
            self.dataLoader = dataLoader
    def numData(self):
        return(self.data.shape[1])
    def predict(self,learningCore,learningAdapter,saveClass,row,col,num=None,addr=None,test=None):
        usedCells = InputMeasurements.loadInfo(self,addr,num)
        res = [] #*self.data.shape[1]
        for i in self.samples:
            if self.keepDataInRam == 1:
                res.append("%.20f" % (learningCore.predict(learningAdapter,self.data[i-1][usedCells])[0]))
                #res.append("%.20f" %  (learningCore.predict(learningAdapter,self.data[i-1][usedCells])[0]))
            else:
                data = np.array(self.dataLoader.loadData('''SELECT value from input where simulation=:sim and sample=:sam order by id''',variables={"sim":str(self.test),"sam":str(i)},dbAddr=self.pomDb),dtype='f').reshape(-1)[usedCells]                     
                #data = np.array(self.dataLoader.loadData('''SELECT * from input where simulation=:sim and sample=:sam''',variables={"sim":str(test),"sam":str(i)},dbAddr=self.pomDb),dtype='f').reshape(-1)[usedCells+2] #create vector (remove first two variables (simulation and sample))                
                data = self.normalize(data,self.dataLoader.getNormDataInput(),usedCells=usedCells)
                res.append("%.20f" %  (learningCore.predict(learningAdapter,data)[0]))
        #print "%i %i %s"%(row,col,res)
        if test == None:
            saveClass.savePredict(res,row,col)
        else:
            saveClass.savePredict(res,row,col,test=test)

class InPreRegions (InputMeasurements):
    def __init__(self,dataLoader,test=None,arguments=None):
        if test == None:
            self.dataOrig = np.array(dataLoader.loadData('''SELECT value FROM measurement order by id'''), dtype='f').reshape(-1) #create vector
        else:
            self.dataOrig = np.array(dataLoader.loadData('''SELECT value FROM output where simulation=:sim order by measurement''',variables={"sim":str(test)}), dtype='f').reshape(-1)
        self.data = self.normalize(self.dataOrig,dataLoader.getNormDataOutput())
    def numData(self):
        return(len(self.data))
    def predict(self,learningCore,learningAdapter,saveClass,row,col,num=None,addr=None,test=None):
        res = str(np.argmax(np.array(learningCore.predict(learningAdapter,self.data)))+1)
        if test == None:
            saveClass.savePredict(res,row,col)
        else:
            saveClass.savePredict(res,row,col,test=test)

class InTest (Input):
    def __init__(self,inClass,dataLoader,arguments=None):            
        self.idTests = np.array(dataLoader.loadData('''SELECT simulation FROM test where test.test=1'''), dtype='i').reshape(-1) #create vector of id tags
        if len(self.idTests) == 0:
            raise Exception('Data for testing not found')
            
        self.tests = []
        for i in self.idTests:
            self.tests.append(inClass.__class__(dataLoader,test=i,arguments=arguments))
    def numData(self):
        return(len(self.tests))
    def predict(self,learningCore,learningAdapter,saveClass,row,col,num=None,addr=None,test=None):
        for i in range(len(self.idTests)):
            self.tests[i].predict(learningCore,learningAdapter,saveClass,row,col,num=num,addr=addr,test=i)
        
    
class Output (Data):
    def save(self,addr,iteration,test=None):
        raise Exception('Implemented by children')
    def saveString(self,data,addr):
        np.savetxt(addr, data, delimiter=",",fmt="%s")
    def saveInt(self,data,addr):
        np.savetxt(addr, data, delimiter=",")
    def savePredict(self,res,row,col,test=None,num=None,addr=None):
        raise Exception('Implemented by children')
    def saveError(self,res,row,col):
        raise Exception('Implemented by children')

class OutputMeasurements(Output):
    pass    
    
class OutMeasurements (OutputMeasurements):
    def __init__(self,data,end=''):
        self.data = deepcopy(data)
        self.end=end
    def save(self,addr,iteration,test=None):
        if test==None:
            addr = "%s/iteration-%i-output%s.csv"%(addr,iteration,self.end)
        else:
             addr = "%s/iteration-%i-test-%i%s.csv"%(addr,iteration,test,self.end)
        Output.saveString(self,self.data,addr)
    def savePredict(self,res,row,col,test=None,num=None,addr=None):
        self.data[row,col] = res
    def saveError(self,res,row,col):
        self.data[row,col] = res
        
class OutBack (OutputMeasurements):
    def __init__(self,data,dataLoader,pomDb):
        self.samples = np.array(dataLoader.loadData('''SELECT DISTINCT sample from input where simulation=-1''',dbAddr=pomDb), dtype='i').reshape(-1)
        self.data = np.array([['NA']*self.samples.shape[0]]*data.shape[0],dtype='S22')
    def save(self,addr,iteration,test=None):
        if test==None:
            addr = "%s/iteration-%i-output-back.csv"%(addr,iteration)
        else:
             addr = "%s/iteration-%i-test-%i-back.csv"%(addr,iteration,test)
        Output.saveString(self,self.data,addr)
    def savePredict(self,res,row,col,test=None,num=None,addr=None):
        self.data[row,:] = res
    def saveError(self,res,row,col):
        self.data[row,:] = res

class OutTest (Output):
    def __init__(self,outClass,dataLoader):
        self.idTests = np.array(dataLoader.loadData('''SELECT simulation FROM test where test.test=1'''), dtype='i').reshape(-1)
        if self.idTests.shape[0] == 0:
            raise Exception('Data for testing not found')
            
        self.tests = []
        for i in self.idTests:
            self.tests.append(deepcopy(outClass))

    def save(self,addr,iteration,test=None):
        for i in range(self.idTests.shape[0]):
            self.tests[i].save(addr,iteration,test=self.idTests[i])
    def savePredict(self,res,row,col,test=None,num=None,addr=None):
        self.tests[test].savePredict(res,row,col)
    def saveError(self,res,row,col):
        for i in self.tests:
            i.saveError(res,row,col)
    
        
