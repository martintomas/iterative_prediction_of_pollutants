#!/bin/bash

HOMEDIR="/storage/brno2/home/martintomas"
DATADIR=$HOMEDIR/ai-cmaq/2-version

#scripts=(pls-corr-red lm-corr3-red neuralnet-corr-red svmpoly-corr-red lm-corr3-red-test krlspoly-corr-red)

#CARET
methods=(lm pls svmPoly avNNet dnn brnn icr kernelpls lars svmRadial)
#methods=(gaussprPoly) #decrease num of CPU, it runs in parallel

#for index in ${methods[*]}
#do
#    qsub -v method=$index -N $index $DATADIR/run-caret.sh
#done

#CARET WITH SNOW 
#methods=(svmPoly gaussprPoly)
#
#for index in ${methods[*]}
#do
#    qsub -v method=$index -N $index $DATADIR/run-caret-snow.sh
#done


#NEURALNET
qsub -N neuralnet $DATADIR/run-neuralnet.sh

#NNET & RSNNS
#methods=(nnet elman) #jordan mlp rbf)

#for index in ${methods[*]}
#do
#    qsub -v method=$index -N $index $DATADIR/run-nnet-rsnns.sh
#done

