#!/bin/bash
#PBS -N iterationRuler
#PBS -l nodes=1:ppn=1
#PBS -l mem=1gb
#PBS -l walltime=2d

# run by typing source main.sh

HOMEDIR="/storage/brno2/home/martintomas/multi-iteration"

source ${HOMEDIR}/environment.set

#CREATE DIRECTORIES                                                                         
mkdir -p $INITDIR
mkdir -p $SAVEDIR
mkdir -p $SCRIPTDIR
mkdir -p $VOLSAVEDIR
mkdir -p $VOLSCRIPTDIR
mkdir -p $CCTMSAVEDIR
mkdir -p $CCTMSAVEDIRR #only selected results
mkdir -p $SOILSAVEDIR

python ${HOMEDIR}/main.py

#run from bash
#MAX=$(($ITERATION + $ITER_FROM))
#while [[ $ITER_FROM -le $MAX ]]
#do
    #echo "$ITER_FROM"
#    qsub -v number=$ITER_FROM $HOMEDIR/iteration.sh
#    (( ITER_FROM++ ))
#done