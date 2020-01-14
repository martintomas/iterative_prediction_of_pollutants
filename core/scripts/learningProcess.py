# -*- coding: utf-8 -*-
"""
Created on Mon Nov  3 13:07:29 2014

@author: trn
"""

import numpy as np

class DataProxy:
    def __init__(self,shape):
        self.data = np.empty(shape,dtype='S14')
    def save(self,row,col,values):
        raise('Implemented by children')
    def initValues(self,data,row=None):
         raise('Implemented by children')
    def getData(self,row=None):
        if row==None:
            return(self.data)
        else:
            return(self.data[row,...])

#shared class
class DataRow (DataProxy):
    pass

#stores test data for one row
class TestRow (DataRow):
    def __init__(self,shape):
        DataProxy.__init__(self,shape)
    def saveTest(self,row,col,values):
        self.data[col,:] = values
    def initValues(self,data,row=None):
        for i in range(self.data.shape[1]):
            self.data[:,i] = data
        
#stores output data for one row
class OutputRow (DataRow):
    def __init__(self,shape):
        DataProxy.__init__(self,shape)
    def saveOutput(self,row,col,values):
        self.data[col] = values
    def initValues(self,data,row=None):
        self.data[:] = data 

#shared class
class DataProcess (DataProxy):
    pass

#stores test data for one row
class TestProcess (DataProcess):
    def __init__(self,shape):
        DataProxy.__init__(self,shape)
    def saveTest(self,row,col,values):
        self.data[row,col,:] = values
    def initValues(self,data,row=None):
        for j in range(self.data.shape[2]):
            self.data[row,:,j] = data
        
#stores output data for one row
class OutputProcess (DataProcess):
    def __init__(self,shape):
        DataProxy.__init__(self,shape)
    def saveOutput(self,row,col,values):
        self.data[row,col] = values
    def initValues(self,data,row=None):
        self.data[row,:] = data 