# -*- coding: utf-8 -*-
"""
Created on Wed Nov 19 15:45:12 2014

@author: trn
"""

import os
from fann2 import libfann
from sklearn import svm, neighbors, tree
import numpy as np

class AdapterLearning:
    def construct(self,*args):
        raise Exception('Implemented by children')
    def train(self,*args):
        raise Exception('Implemented by children')
    def predict(self,*args):
        raise Exception('Implemented by children')
        
class SklearnLearning(AdapterLearning):    
    def train(self,*args):
        svm = args[0]   
        fileSample = args[1]
        [x,y] = self.readFile(fileSample)
        svm.fit(x, y) 
    def predict(self,*args):
        svm = args[0]
        data = args[1]
        return(svm.predict([data]))
    def readFile(self,fileSample):
        fileSample = open(fileSample, 'r')
        fileSample.readline() #skip first line
        x = [] #input
        y = [] #output
        switch = False 
        for i in fileSample: #read rest
            if switch:
                y.append(i.rstrip('\n').split(' '))
            else:
                x.append(i.rstrip('\n').split(' '))
            switch = not switch
        return([np.array(x,dtype='f'),np.array(y,dtype='f').reshape(-1)])
        

class SVMRegression(SklearnLearning):
    def construct(self,*args):
        clf = svm.SVR(C=1.5,gamma=0.2)
        return(clf)

class Neighbors(SklearnLearning):
    def construct(self,*args):
        knn = neighbors.KNeighborsRegressor()
        return(knn)

class Tree(SklearnLearning):
    def construct(self,*args):
        knn = tree.DecisionTreeRegressor()
        return(knn)
        
class FannLearning(AdapterLearning):
    def train(self,*args):
        ann = args[0]      
        fileSample = args[1]
        ann.train_on_file(fileSample, self.max_iterations, self.iterations_between_reports, self.desired_error)
    def predict(self,*args):
        ann = args[0]
        data = args[1]
        return(ann.run(data))

class FannLearningRegression(FannLearning):
    def __init__(self): #set up for this module
        self.connection_rate = 1
        self.learning_rate = 0.7
        self.num_hidden = 5
        self.desired_error = 0.0001
        self.max_iterations = 5000
        self.iterations_between_reports = 5000
    def construct(self,*args):
        num_input = args[0]
        num_output = args[1]
        ann = libfann.neural_net()
        ann.create_sparse_array(self.connection_rate, (num_input, self.num_hidden, num_output))
        ann.set_learning_rate(self.learning_rate)
        ann.set_activation_function_hidden(libfann.SIGMOID_SYMMETRIC_STEPWISE)
        ann.set_activation_function_output(libfann.GAUSSIAN)
        return(ann)

class FannLearningClassification(FannLearning):
    def __init__(self): #set up for this module
        self.connection_rate = 1
        self.num_hidden = 2
        self.desired_error = 0.0001
        self.max_iterations = 1000
        self.iterations_between_reports = 1000
    def construct(self,*args):
        num_input = args[0]
        num_output = args[1]
        ann = libfann.neural_net()
        ann.create_standard_array((num_input, self.num_hidden, num_output))
        ann.set_activation_function_hidden(libfann.SIGMOID)
        #ann.set_activation_function_output(libfann.SIGMOID_SYMMETRIC)
        ann.set_training_algorithm(libfann.TRAIN_QUICKPROP)
        return(ann)
    


class LearningFactory:
    def __init__(self):
        self.learningType=os.environ['LEARNINGSETUP']
        self.swModule=os.environ['LEARNINGMODULE']
    def getLearningModule(self):
        if self.swModule=='FANN':
            if self.learningType == 'regression':
                return(FannLearningRegression())
            else:
                return(FannLearningClassification())
        elif self.swModule=='SVM':
            if self.learningType == 'regression':
                return(SVMRegression())
            else:
                raise Exception('Not implemented')
                #return(FannLearningClassification())
        elif self.swModule=='Neighbors':
            if self.learningType == 'regression':
                return(Neighbors())
            else:
                raise Exception('Not implemented')
                #return(FannLearningClassification())
        elif self.swModule=='Tree':
            if self.learningType == 'regression':
                return(Tree())
            else:
                raise Exception('Not implemented')
                #return(FannLearningClassification())
