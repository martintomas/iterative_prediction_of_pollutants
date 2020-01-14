#!/bin/bash
#PBS -N learning-cmaq
#PBS -l nodes=1:ppn=16:xeon
#PBS -l mem=5gb
#PBS -l scratch=5gb:local
#PBS -l walltime=2d

# run by typing source main-learning.sh

if [ -z "$ITERATION" ] ; then
   echo "Number of iteration is missing!!"
   export ITERATION=1
fi

HOMEDIR="/storage/brno2/home/martintomas/multi-iteration"
ENVFILE="environment.set"

#for testing
#OUTDIR="11-region-dist-nonuniform" #for testing
#echo $OUTDIR

module add scipy-0.12.0-py2.7
export PYTHONPATH=/storage/brno2/home/martintomas/CMAQv5.0.1/libs/python/lib/python2.7/site-packages/sklearn:$PYTHONPATH

#cp -r ${HOMEDIR}/save/learning/${OUTDIR}/* ${SCRATCHDIR}/

source ${HOMEDIR}/${ENVFILE}
module add openmpi-1.6-intel

export OMP_NUM_THREADS=$PBS_NUM_PPN

#mkdir -p $SCRATCHDIR
#cd $SCRATCHDIR || exit 2

# clean scratch
trap 'clean_scratch' TERM EXIT

#CREATE DIRECTORIES                                                                         
mkdir -p $SAVESCRATCH
mkdir -p $SAVEDIR
mkdir -p $SOILSAVEDIR
mkdir -p $LEARNINGSAVEDIR

export LEARNINGSETUP="regression" #learning sw can be usually used in regression or classification version (regression is default one)

if [ $USEREGPREPARATION -eq 1 ] ; then

  if ([ $USEONLYONCE -eq 1 ] && [ $ITERATION -eq 1 ]) || [ $USEONLYONCE -eq 0 ] ; then

    PTYPELEARNING=$TYPELEARNING

    if [ "$METHOD3" == "classification" ] ; then
    	export TYPELEARNING="preregions-clas" #prepare regions
	export LEARNINGSETUP="classification" #change to classification version
    else 
    	export TYPELEARNING="preregions-reg" #prepare regions
    fi


    if [ "$METHOD3" == "analysis" ] ; then #skip learning, only compute analysis regions
	echo "Region learning is going to be skipped"
	Rscript ${SCRIPTDIR}/prepare-regions.r ${INITDIR}/${LEARNINGDB} ${SCRATCHDIR} ${INITDIR}/analysis.csv $SCRIPTDIR ${INITDIR}/${INITSOIL} $MAXSIM $MINREGIONS $MAXREGIONS $ITERATION
    else
    	python ${HOMEDIR}/main-learning.py
    fi
    
    #save results
    mkdir -p ${LEARNINGSAVEDIR}/iteration-${ITERATION}-method-${TYPELEARNING}
    cp ${SAVESCRATCH}/* ${LEARNINGSAVEDIR}/iteration-${ITERATION}-method-${TYPELEARNING}/

    export TYPELEARNING=$PTYPELEARNING

  fi

fi

#if [ "$METHOD3" == "analysis" ] || [ $USEREGPREPARATION -eq 0 ] || [ $SHORTENPRED -eq 0 ] || ([ $SHORTENPRED -eq 1 ] && [ $USEONLYONCE -eq 1 ] && [ $ITERATION -gt 1 ]) ; then
if [ "$METHOD3" == "analysis" ] || !([ $USEREGPREPARATION -eq 1 ] && [ $SHORTENPRED -eq 1 ] && (([ $USEONLYONCE -eq 1 ] && [ $ITERATION -eq 1 ]) || [ $USEONLYONCE -eq 0 ])) ; then

  python ${HOMEDIR}/main-learning.py

fi

#save results
mkdir -p ${LEARNINGSAVEDIR}/iteration-${ITERATION}-method-${TYPELEARNING}
cp ${SAVESCRATCH}/* ${LEARNINGSAVEDIR}/iteration-${ITERATION}-method-${TYPELEARNING}/

#for testing
#mkdir -p ${LEARNINGSAVEDIR}/${OUTDIR}
#cp -r ${SCRATCHDIR}/* ${LEARNINGSAVEDIR}/${OUTDIR}/
#cp ${HOMEDIR}/${ENVFILE} ${LEARNINGSAVEDIR}/${OUTDIR}/

if [ $USEREVERSELEARNING -eq 1 ] ; then

  echo "Using reverse learning method"

  export PTYPESELECT=$TYPESELECT #used to obtain information from first run
  export PTYPELEARNING=$TYPELEARNING

  #set up variables for reverse learning (so we can use same python script)
  export MINDATA=$RMINDATA
  export MAXDATA=$RMAXDATA
  export TESTING=$RTESTING
  export TYPESELECT=$RTYPESELECT
  export TYPELEARNING="back" #for reverse learning, only cell prediction can be used

  if [ $MAXSIM -gt $RNUM ] ; then

    echo "RNUM should be bigger then MAXSIM!"
    export RNUM=$MAXSIM

  fi
    
  #run main script
  python ${HOMEDIR}/main-learning.py

  #save results
  mkdir -p ${LEARNINGSAVEDIR}/iteration-${ITERATION}-method-${TYPELEARNING}
  cp ${SAVESCRATCH}/* ${LEARNINGSAVEDIR}/iteration-${ITERATION}-method-${TYPELEARNING}/

  #for testing
  #mkdir -p ${LEARNINGSAVEDIR}/${OUTDIR} 
  #cp -r ${SCRATCHDIR}/* ${LEARNINGSAVEDIR}/${OUTDIR}/
  #cp ${HOMEDIR}/${ENVFILE} ${LEARNINGSAVEDIR}/${OUTDIR}/
fi

#save ncdf files
cp ${SAVESCRATCH}/*.nc ${SOILSAVEDIR}/
