# -*- coding: utf-8 -*-
"""
Created on Wed Nov  5 12:39:58 2014

@author: trn

"""

from subprocess import call
import os


class RLearning:
    def __init__(self,scratchdir,saveScratch,iteration,cores,initDir,learningDB,testing): #version == if process per row or process per group of row is used
        self.scratchdir = scratchdir
        self.saveScratch = saveScratch
        self.iteration = iteration
        self.cores = cores
        self.initDir = initDir
        self.scriptDir = os.environ['SCRIPTDIR']
        self.learningDB = learningDB
        self.maxSim = int(os.environ['MAXSIM'])
        self.minData = os.environ['MINDATA']
        self.maxData = os.environ['MAXDATA']
        self.maxNA = int(os.environ['MAXNA']) #in %
        self.method1 = os.environ['METHOD1'] #can be add or noadd
        self.method2 = os.environ['METHOD2'] #can be nonuniform or uniform
        self.typeSelect = os.environ['TYPESELECT'] #can be dist, corr or all
        self.type = os.environ['TYPELEARNING'] #can be cell or region or regions
        self.initSoil = os.environ['INITSOIL']
        self.testing = testing
        self.mpi = 1 #0 == use Rscript, 1 == use mpirun
        self.mapR = self.createMap()
        self.dbAddr = "%s/%s"%(self.initDir,self.learningDB) #db location
        self.devnull = open(os.devnull, 'wb')
    
    def createMap(self):
        mapR = {"cell":{"preprocess":self.preProcessCell,"samples":self.samplesCell,"postprocess":self.postProcessCell}, \
                "region":{"preprocess":self.preProcessRegion,"samples":self.samplesCell,"postprocess":self.postProcessCell}, \
                "back":{"preprocess":self.preProcessBack,"samples":self.samplesBack,"postprocess":self.postProcessBack}, \
                "preregions-clas":{"preprocess":self.preProcessPreRegions,"samples":self.samplesPreRegions,"postprocess":self.postProcessPreRegions}, \
                "preregions-reg":{"preprocess":self.preProcessPreRegionsReg,"samples":self.samplesPreRegionsReg,"postprocess":self.postProcessPreRegionsReg}, \
                "regions":{"preprocess":self.preProcessRegions,"samples":self.samplesRegions,"postprocess":self.postProcessRegions}}
        return(mapR)
    def preProcess(self):
        self.mapR[self.type]["preprocess"]()
    def samples(self,row,cols=None):
        self.mapR[self.type]["samples"](row,cols)
    def postProcess(self):
        self.mapR[self.type]["postprocess"]()   
    def preProcessRegion(self):
        #run preparation R scripts
        print('Creating secondary database with helpful data for selection split')
        call("%s %s/pom-data.db %s %s %s/map-regions.csv %i" % (self.parPrefix(self.scriptDir,'create-pom-db-region.r'),self.scratchdir,self.dbAddr,self.typeSelect,self.initDir,self.cores), shell=True)
        print('Creating prediction map (map.csv, steps.csv or steps.db)')
        call("Rscript %s/create-maps.r %s/pom-data.db %s/analysis.csv %s %i %s %s %i %s %s %s %s" % (self.scriptDir,self.scratchdir,self.initDir,self.scratchdir,self.maxSim,self.minData,self.maxData,self.maxNA,self.typeSelect,self.method1,self.method2,self.type), shell=True)
    def preProcessCell(self):
        #run preparation R scripts
        print('Creating secondary database with helpful data for selection split')
        call("%s %s/pom-data.db %s %s/analysis.csv %s %i %s" % (self.parPrefix(self.scriptDir,'create-pom-db.r'),self.scratchdir,self.dbAddr,self.initDir,self.typeSelect,self.cores,self.type), shell=True)
        print('Creating prediction map (map.csv, steps.csv or steps.db)')
        call("Rscript %s/create-maps.r %s/pom-data.db %s/analysis.csv %s %i %s %s %i %s %s %s %s" % (self.scriptDir,self.scratchdir,self.initDir,self.scratchdir,self.maxSim,self.minData,self.maxData,self.maxNA,self.typeSelect,self.method1,self.method2,self.type), shell=True)
    def samplesCell(self,row,col=None):
        if col == None:
            call("Rscript %s/create-sample2.r %s %s/pom-data.db %s/map.csv %s sample %s %s %s %s %s %s %s/%s %s/map-regions.csv" % (self.scriptDir,self.dbAddr,self.scratchdir,self.scratchdir,self.scratchdir,self.scriptDir,row,self.typeSelect,self.method1,self.method2,self.type,self.initDir,self.initSoil,self.initDir), shell=True,stdout=self.devnull)
        else:
            #Not used now
            call("Rscript %s/create-sample.r %s %s/pom-data.db %s sample %s %s %s %s %s" % (self.scriptDir,self.dbAddr,self.scratchdir,self.scratchdir,row+1,col,self.typeSelect,self.method1,self.method2), shell=True, stdout=self.devnull, stderr=self.devnull)
    def postProcessCell(self):           
        print('Plotting test data (if wanted) and preparing ncdf files')
        call("Rscript %s/post-learning.r %s %s %s %s %s/analysis.csv %s/%s %s/iteration-%i-info.txt %i %i %i %s %s/map-regions.csv"%(self.scriptDir,self.dbAddr,self.saveScratch,self.scriptDir,self.saveScratch,self.initDir,self.initDir,self.initSoil,self.saveScratch,self.iteration,self.iteration,self.testing,self.startSimNumber(),self.type,self.initDir),shell=True)
        print('Checking if enough files were created') 
        #parallelized version              
        #call("%s %s/init-soil %s/analysis.csv %s %i %i %i %i" % (self.parPrefix(self.scriptDir,'check-inputs-par.r'),self.saveScratch,self.initDir,self.scriptDir,self.startSimNumber(),self.endSimNumber(),self.iteration,self.cores),shell=True)
        #non parallelized version       
        useReverseLearning = int(os.environ['USEREVERSELEARNING'])
        if useReverseLearning == 0: #check if enough samples was obtained (if is used reverse learning, it is not needed)
            call("Rscript %s/check-inputs.r %s/init-soil %s/analysis.csv %s %i %i %i" % (self.scriptDir,self.saveScratch,self.initDir,self.scriptDir,self.startSimNumber(),self.endSimNumber(),self.iteration),shell=True)
        
    def preProcessBack(self):
        rNum = int(os.environ['RNUM'])
        pType = os.environ['PTYPELEARNING']
        pTypeSelect = os.environ['PTYPESELECT']
        useKriging = int(os.environ['USEKRIGING'])
        print('Generating new samples for reverse prediction')
        call("%s %s %s/analysis.csv %s %s/pom-data-back.db %s %i %i %i %i %i" % (self.parPrefix(self.scriptDir,'back-generation.r'),self.saveScratch,self.initDir,self.scriptDir,self.scratchdir,self.dbAddr,self.startSimNumber(),self.startSimNumber()+rNum-1,self.cores,self.testing,useKriging),shell=True)
        print('Creating secondary database with helpful data')        
        call("%s %s/pom-data.db %s %s/analysis.csv %s %i %s %s %s" % (self.parPrefix(self.scriptDir,'create-pom-db.r'),self.scratchdir,self.dbAddr,self.initDir,self.typeSelect,self.cores,self.type,pType,pTypeSelect), shell=True)     
        print('Samples generating')
        call("%s %s %s/pom-data.db %s sample %s/analysis.csv %s %s %s %s %i" % (self.parPrefix(self.scriptDir,'create-sample-back.r'),self.dbAddr,self.scratchdir,self.scratchdir,self.initDir,self.scriptDir,self.typeSelect,self.minData,self.maxData,self.cores),shell=True)
        
    def samplesBack(self,row,col=None): #it is already created in preprocess
        pass
    def postProcessBack(self):
        print('Running post process after reverse learning.')
        call("Rscript %s/post-learning-back.r %s %s %s %s %s/analysis.csv %s/%s %s/iteration-%i-info-back.txt %i %i %i %i %s %s/pom-data-back.db"%(self.scriptDir,self.dbAddr,self.saveScratch,self.scriptDir,self.saveScratch,self.initDir,self.initDir,self.initSoil,self.saveScratch,self.iteration,self.iteration,self.testing,self.startSimNumber(),self.endSimNumber(),self.type,self.scratchdir),shell=True)
    
    def preProcessPreRegions(self):
        minRegions = int(os.environ['MINREGIONS'])
        maxRegions = int(os.environ['MAXREGIONS'])
        print('Creating regions before learning process')
        call("Rscript %s/prepare-regions.r %s %s %s/analysis.csv %s %s/%s %i %i %i %i"%(self.scriptDir,self.dbAddr,self.scratchdir,self.initDir,self.scriptDir,self.initDir,self.initSoil,self.maxSim,minRegions,maxRegions,self.iteration),shell=True)
    
    def samplesPreRegions(self,row,col=None):
        call("Rscript %s/create-sample-preregions.r %s %s %s %s/%s %s"%(self.scriptDir,self.dbAddr,self.scratchdir,self.scriptDir,self.initDir,self.initSoil,row),shell=True)        
        pass
    
    def postProcessPreRegions(self):
        minRegions = int(os.environ['MINREGIONS'])
        maxRegions = int(os.environ['MAXREGIONS'])
        shorten = int(os.environ['SHORTENPRED'])
        print('Finishing region creation')
        call("Rscript %s/post-learning-preregions.r %s %s %s %s %s %s/analysis.csv %s/iteration-%i-info-preregions.txt %i %i %s %i %i %i %i %s/%s %i"%(self.scriptDir,self.dbAddr,self.saveScratch,self.scriptDir,self.scratchdir,self.saveScratch,self.initDir,self.saveScratch,self.iteration,self.iteration,self.testing,self.type,shorten,self.maxSim,minRegions,maxRegions,self.initDir,self.initSoil,self.startSimNumber()),shell=True)
        pass
    
    def preProcessPreRegionsReg(self):
        print('Creating dummy region')
        call("Rscript %s/dummy-region.r %s %s/dummy-region.csv %s/analysis.csv"%(self.scriptDir,self.dbAddr,self.scratchdir,self.initDir),shell=True)
        print('Creating secondary database with helpful data for selection split')
        call("%s %s/pom-data.db %s %s %s/dummy-region.csv %i" % (self.parPrefix(self.scriptDir,'create-pom-db-region.r'),self.scratchdir,self.dbAddr,'all',self.scratchdir,self.cores), shell=True)
        print('Creating prediction map (map.csv, steps.csv or steps.db)')
        call("Rscript %s/create-maps.r %s/pom-data.db %s/analysis.csv %s %i %s %s %i %s %s %s %s" % (self.scriptDir,self.scratchdir,self.initDir,self.scratchdir,self.maxSim,self.minData,self.maxData,self.maxNA,'all',self.method1,self.method2,'region'), shell=True)
    
    def samplesPreRegionsReg(self,row,col=None):
        call("Rscript %s/create-sample2.r %s %s/pom-data.db %s/map.csv %s sample %s %s %s %s %s %s %s/%s %s/dummy-region.csv" % (self.scriptDir,self.dbAddr,self.scratchdir,self.scratchdir,self.scratchdir,self.scriptDir,row,'all',self.method1,self.method2,'region',self.initDir,self.initSoil,self.scratchdir), shell=True,stdout=self.devnull)
    
    def postProcessPreRegionsReg(self):
        print('Preparing regions (denormalization, etc.)')
        call("Rscript %s/post-learning.r %s %s %s %s %s/analysis.csv %s/%s %s/iteration-%i-info-regions.txt %i %i %i %s %s/dummy-region.csv"%(self.scriptDir,self.dbAddr,self.saveScratch,self.scriptDir,self.saveScratch,self.initDir,self.initDir,self.initSoil,self.saveScratch,self.iteration,self.iteration,self.testing,self.startSimNumber(),'region',self.scratchdir),shell=True)
        self.postProcessPreRegions()
    
    def preProcessRegions(self):
        regionDir = os.environ['REGIONSDIR']
        if not os.path.exists("%s/regions.csv"%regionDir):
            raise Exception('File regions.csv was not found!!!')
        print('Creating secondary database with helpful data for selection split')
        call("%s %s/pom-data.db %s %s/analysis.csv %s %i %s" % (self.parPrefix(self.scriptDir,'create-pom-db.r'),self.scratchdir,self.dbAddr,self.initDir,self.typeSelect,self.cores,'cell'), shell=True) #use cell, we compute different regions from it
        print('Creating prediction map (map.csv, !!! it requires regions.csv file)')
        call("Rscript %s/create-map-regions.r %s/regions.csv %s/pom-data.db %s %s %s %s %s"%(self.scriptDir,regionDir,self.scratchdir,self.scriptDir,self.scratchdir,self.minData,self.maxData,self.typeSelect),shell=True)
    
    def samplesRegions(self,row,col=None):
        regionDir = os.environ['REGIONSDIR']
        call("Rscript %s/create-sample-regions.r %s %s/pom-data.db %s/map.csv %s/sample %s %s/%s %s/regions.csv %s %s %s %s"%(self.scriptDir,self.dbAddr,self.scratchdir,self.scratchdir,self.scratchdir,self.scriptDir,self.initDir,self.initSoil,regionDir,self.minData,self.maxData,self.typeSelect,row),shell=True,stdout=self.devnull)
    
    def postProcessRegions(self):     
        regionDir = os.environ['REGIONSDIR']
        print('Plotting test data (if wanted) and preparing ncdf files')
        call("Rscript %s/post-learning.r %s %s %s %s %s/analysis.csv %s/%s %s/iteration-%i-info.txt %i %i %i %s %s/regions.csv"%(self.scriptDir,self.dbAddr,self.saveScratch,self.scriptDir,self.saveScratch,self.initDir,self.initDir,self.initSoil,self.saveScratch,self.iteration,self.iteration,self.testing,self.startSimNumber(),self.type,regionDir),shell=True)
    
    def parPrefix(self,scriptDir,script):
        if self.mpi == 1:
            return("mpirun -np 1 R --no-save -q < %s/%s --args"%(scriptDir,script))
        else:
            return("Rscript %s/%s "%(scriptDir,script))
    
    def startSimNumber(self):
        simFrom = int(os.environ['SIMFROM'])
        firstMaxSim = int(os.environ['FIRSTMAXSIM'])
        if(self.iteration==0):
            return(0+simFrom)
        else:
            return(firstMaxSim+((self.iteration-1)*self.maxSim)+simFrom)
            
    def endSimNumber(self):
        firstMaxSim = int(os.environ['FIRSTMAXSIM'])
        if(self.iteration==0):
            return(self.startSimNumber()+firstMaxSim-1)
        else:
            return(self.startSimNumber()+self.maxSim-1)
    
    #check if used method is known
    def checkTypeLearning(self):
        return(self.typeSelect in ('dist','corr','all'))
