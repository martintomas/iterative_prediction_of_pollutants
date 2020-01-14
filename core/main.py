'''
Created on Jul 10, 2014

@author: trn
'''

import os
from subprocess import Popen, PIPE, call
import thread
import time
import datetime
import re
from xml.etree.ElementTree import fromstring, ElementTree

#load variables
maxSim=int(os.environ['MAXSIM'])
firstMaxSim=int(os.environ['FIRSTMAXSIM'])
iterTo=int(os.environ['ITERTO'])
iterFrom=int(os.environ['ITERFROM']) - 1 #!! don't forget to minus 1
simFrom=int(os.environ['SIMFROM'])
numday=int(os.environ['NUMDAY']) #4 #how many days, to check that we have obtained enough files
firstDay=("%s-%s-%s"%(os.environ['YEAR'],os.environ['MONTH'],os.environ['DAY']))

homedir=os.environ['HOMEDIR'] #"/home/trn/diploma-thesis/software/multi-iteration"
cctmSaveDirR=os.environ['CCTMSAVEDIRR'] #we check only extraction #"/home/trn/diploma-thesis/software/multi-iteration/test/files"
soilSaveDir=os.environ['SOILSAVEDIR']
scriptDir=os.environ['SCRIPTDIR']
mcipDir=os.environ['MCIPDIR']
initDir=os.environ['INITDIR']

metFile=os.environ['METFILE']
initGrid=os.environ['INITGRID']
initObserv=os.environ['INITOBSERV']
initSoil=os.environ['INITSOIL']
initSfoc=os.environ['INITSFOC']
initTSoil=os.environ['INITTSOIL']
learningDB=os.environ['LEARNINGDB']
aconName=os.environ['ACONNAME']

cores = int(os.environ['CORES'])

def startNewSimulation(number):
    p = Popen("qsub -v number=%i %s/run.multi.sh" % (number,homedir), stdout=PIPE, stderr=PIPE,shell=True)
    #p = Popen("python %s/test/new-simulation.py %i" % (homedir,number), stdout=PIPE, stderr=PIPE, shell=True)
    stdout, stderr = p.communicate()
    return(int(stdout.split('.')[0]))

def startNewLearning(iteration,cores):
    p = Popen("qsub -v ITERATION=%i %s/main-learning.sh -l nodes=1:ppn=%i -l mem=5gb -l scratch=5gb:local" % (iteration,homedir,cores), stdout=PIPE, stderr=PIPE,shell=True)
    stdout, stderr = p.communicate()
    return(int(stdout.split('.')[0]))

def recoverSimulation(number,days):
    p = Popen("qsub -v number=%i,Cdays=%i %s/run.multi.sh" % (number,days,homedir), stdout=PIPE, stderr=PIPE,shell=True)
    #p = Popen("python %s/test/new-simulation.py %i" % (homedir,number), stdout=PIPE, stderr=PIPE, shell=True)
    stdout, stderr = p.communicate()
    return(int(stdout.split('.')[0]))    

#TRUE==FINISHED
#FALSE=RUNINNG
def checkRunningSimulation(jobID):
    p = Popen("qstat -x %s" % (jobID), stdout=PIPE, stderr=PIPE,shell=True)
    #p = Popen("python %s/test/check-job.py" % (homedir), stdout=PIPE, stderr=PIPE, shell=True)
    stdout, stderr = p.communicate()
    try:
        root = ElementTree(fromstring(stdout))
        for data in root.findall('Job'):
            state = data.find('job_state').text
    except Exception:
        print('Error occured during checking if job is running. Id of job is %s'%(jobID))
        return(True)
    return(state=='C') #C==completed

#RETURNS NUMBER OF FILES
def checkFiles(number,checkDir,reg):    
    onlyfiles = [ f for f in os.listdir(checkDir) if re.match(reg, f) ]
    return(len(onlyfiles))
    
#RETURNS NUMBER OF CMAQ OUTPUT FILES
def checkSimulationFiles(number):
    reg = (r'.*\_%i\.ncf$' % (number)) 
    return(checkFiles(number,cctmSaveDirR,reg))

def startSimNumber(iteration):
    if(iteration==0):
        return(0+simFrom)
    else:
        return(firstMaxSim+((iteration-1)*maxSim)+simFrom)
        
def endSimNumber(iteration,first):
    if(iteration==0):
        return(first+firstMaxSim-1)
    else:
        return(first+maxSim-1)

#mapOfIterations:
#-2 -> not runing
#-1 -> finished
#positive integer -> runing #integer=num of job
def cmaqSimulations(iteration,sim):
    firstSimId = startSimNumber(iteration)
    mapOfIterations = [-2 for x in range(sim)]
    
    for i in range(sim): #start all simulations
        print "Running new simulation of number",firstSimId+i
        mapOfIterations[i] = startNewSimulation(firstSimId+i) #return num of job
    
    finished = 0
    while(finished!=sim): #check running nodes        
        indexes = [i for i,val in enumerate(mapOfIterations) if val>0] #positive integer == running job 
        for i in indexes:
            if(checkRunningSimulation(mapOfIterations[i])): #is finished
                print ("Simulation of number %s has finished" % (firstSimId+i))
                finishedDays = checkSimulationFiles(firstSimId+i)
                if(finishedDays==numday):
                    mapOfIterations[i]=-1
                    finished+=1
                    print ("Simulation of number %s has right number of files" % (firstSimId+i))
                else:
                    print ("Simulation of number %s has wrong number of files, running again" % (firstSimId+i))
                    if(finishedDays>0): #can use old CGRID file
                        mapOfIterations[i] = recoverSimulation(firstSimId+i,finishedDays) #rerun part of the simulation
                    else:
                        mapOfIterations[i] = startNewSimulation(firstSimId+i) #rerun whole simulation
            #else:
                #print ("Simulation of number %s is still running" % (firstSimId+i))
                
        time.sleep(60)
    
    
if __name__ == '__main__':
            
    while(iterFrom<iterTo):
        if(iterFrom==0):
            #here will be analysis + map creation + temperature mean
            #first iteration, do random input initialization    
            #print("Computing temperature mean")
            call("Rscript %s/temp-mean.r %s %s/temp-mean.nc %s %s" % (scriptDir,mcipDir,initDir,scriptDir,metFile), shell=True)
            print("Running analysis of data")
            call("Rscript %s/analysis.r %s %s %s temp-mean.nc analysis.nc %s" % (scriptDir,initDir,initGrid,initObserv,scriptDir), shell=True)
            print("Creating analysis.csv and map-regions.csv")
            call("Rscript %s/region-map.r %s analysis.nc %s %s %s" % (scriptDir,initDir,initSoil,initSfoc,initTSoil), shell=True)
            print("Generating new random inputs")
            call("Rscript %s/soil-first-iteration.r %s %s analysis.csv %s/init-soil %s %s %i" % (scriptDir,initDir,initSoil,soilSaveDir,scriptDir,maxSim,simFrom), shell=True)
            
            #prepare db for learning
            print("Initialization of SQL db")
            call("Rscript %s/create-learning-db.r %s/%s %s/%s %s" % (scriptDir,initDir,learningDB,initDir,initObserv,scriptDir), shell=True)
            
            #do CMAQ simulations
            cmaqSimulations(iterFrom,firstMaxSim)
        else:
            cmaqSimulations(iterFrom,maxSim)
        
        #collect data
        startSimId = startSimNumber(iterFrom)
        endSimId = endSimNumber(iterFrom,startSimId)
        call("Rscript %s/collect-data.r %s/%s %s/init-soil %s/%s %s/%s %s %i %i %s %i" % (scriptDir,initDir,learningDB,soilSaveDir,cctmSaveDirR,aconName,initDir,initObserv,scriptDir,startSimId,endSimId,firstDay,numday), shell=True)
        
        #predict new inputs
        idLearning = startNewLearning(iterFrom+1,cores)
        while(not checkRunningSimulation(idLearning)):
            #waiting for learning to finish
            time.sleep(60)
            
        print("Computing some summarization information before iteration ends")
        call("Rscript %s/summarization.r %s/%s %s %s %i" % (scriptDir,initDir,learningDB,homedir,scriptDir,iterFrom), shell=True)
        
        iterFrom+=1
    exit()
