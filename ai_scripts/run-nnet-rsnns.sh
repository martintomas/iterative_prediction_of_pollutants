#!/bin/bash
#PBS -l nodes=1:ppn=15
#PBS -l mem=20gb
#PBS -l walltime=3d
# direktivy si upravte/umazte dle potreb sveho vypoctu

#CPUNUM= #$PBS_JOBCOOKIE #$PBS_VNODENUM
export OMP_NUM_THREADS=$PBS_NUM_PPN
export OMPI_MCA_btl=sm,tcp,self
export OMPI_UNIVERSE_SIZE=`expr $TORQUE_RESC_TOTAL_PROCS - 1`
#method="" #!!! remove when used rule script

#we need number variable to read and copy right data values
if [ -z "$method" ] ; then
echo "Used RSNNS/nnet method for script is missing"
exit 1
fi

HOMEDIR="/storage/brno2/home/martintomas"
source $HOMEDIR/first-iteration/environment-r.set

DATADIR=$HOMEDIR/ai-cmaq
NUMBER=4
REDUC=5000
script="nnet-rsnns"

#norm2
name="" #begin with -
#mpirun -np 1 R --no-save -q < $DATADIR/2-version/$script --args $DATADIR/input-$name.csv $DATADIR/output-$name.csv $DATADIR/output-$name-reduced-$REDUC.csv $DATADIR/2-version -$name-$REDUC-$NUMBER $DATADIR/corr-$name.db $DATADIR/input-$name-back.csv  
mpirun -np 1 R --no-save -q < $DATADIR/2-version/$script --args $DATADIR/input$name.csv $DATADIR/output$name.csv $DATADIR/output$name-reduced-$REDUC.csv $DATADIR/2-version $name-$REDUC-$NUMBER $DATADIR/corr$name.db $DATADIR/input$name-back.csv $method $DATADIR/init-output.csv

REDUC=200
mpirun -np 1 R --no-save -q < $DATADIR/2-version/$script --args $DATADIR/input$name.csv $DATADIR/output$name.csv $DATADIR/output$name-reduced-$REDUC.csv $DATADIR/2-version $name-$REDUC-$NUMBER $DATADIR/corr$name.db $DATADIR/input$name-back.csv $method $DATADIR/init-output.csv
