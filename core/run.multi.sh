#!/bin/bash
#PBS -N cmaq
#PBS -l nodes=1:ppn=8
#PBS -l mem=2gb
#PBS -l scratch=10gb:local
#PBS -l walltime=1d

#> Script for multiday run of the std script
#> Build loop to set the duration of the run with sed command 

if [ -z "$number" ] ; then
    echo "Number of simulation is missing"
    number="" #set empty string
    #exit 1
else
    number="_${number}" 
fi

#CONNECT THIS SCRIPT WITH REST OF THE APP
HOMEDIR="/storage/brno2/home/martintomas/multi-iteration"
source ${HOMEDIR}/environment.set

export OMP_NUM_THREADS=$PBS_NUM_PPN

mkdir -p $SCRATCHDIR
cd $SCRATCHDIR || exit 2

# clean scratch
trap 'clean_scratch' TERM EXIT

source ${HOMECMAQDIR}/environment-CMAQ.set
mkdir -p $SCRATCHDIR/data
export M3DATA=$SCRATCHDIR/data #data are taken from scratch

#COPY FIRST CMAQ DATA
echo "Copying data to scratch"
cp -r ${HOMECMAQDIR}/data/raw ${SCRATCHDIR}/data  || exit 1
cp -r ${HOMECMAQDIR}/data/procan ${SCRATCHDIR}/data  || exit 1
mkdir ${SCRATCHDIR}/data/emis
cp ${HOMECMAQDIR}/data/emis/NEWG ${SCRATCHDIR}/data/emis/  || exit 1
mkdir ${SCRATCHDIR}/data/mcip3
mkdir ${SCRATCHDIR}/data/cgrid #previous cctm cgrid files are stored there
#chmod -R 777 $SCRATCHDIR/*

#COPY VOLATILIZATION FILES
if [ $VOLATILIZATION ] ; then
	mkdir ${SCRATCHDIR}/volatilization
	mkdir ${SCRATCHDIR}/init
	cp ${INITDIR}/${INITSOIL} ${SCRATCHDIR}/init/ || exit 1 #WILL COPY ONLY INIT SOIL CREATED BY RANDOM FUNCTION OR LEARNING MODULE
	cp ${INITDIR}/${INITSFOC} ${SCRATCHDIR}/init/ || exit 1
fi

FIRSTDATE=${YEAR}${MONTH}${DAY}
END=$(( $NUMDAY - 1 ))

#REFRESHING SIMULATION -- USE OLD CGRID FILE
if [ -n "$Cdays" ] ; then 	#Cdays == number of already simulated days
    echo "Already simulated ${Cdays} days. Simulation is going to be refreshed from this point."
    JDAY=$(( $Cdays - 1 )) #previous day
    export DMD=`date -d "${FIRSTDATE} + ${JDAY} day" +%Y%j`
    FIRSTDATE=`date -d "${FIRSTDATE} + ${Cdays} day" +%Y%m%d` #recompute first day
    END=$(( $END - $Cdays )) #recompute number of steps
    cp ${CCTMSAVEDIR}/${CGRIDNAME}.${DMD}${number}.ncf ${SCRATCHDIR}/data/cgrid/${CGRIDNAME}.${DMD}.ncf #copy CGRID file to scratch
fi

for x in `seq 0 1 $END`;
do
	export STDATE=`date -d "${FIRSTDATE} + ${x} day" +%Y%j`  #used by jproc and cctm 
	export SJDATE=`date -d "${FIRSTDATE} + ${x} day" +%j`
	
	#COPY EMIS AND MCIP DATA BEFORE SIMULATION
	echo "Copying emis file to scratch"
	cp ${EMISDIR}/${EMISNAME}_${SJDATE} ${SCRATCHDIR}/data/emis/  || exit 1 #copy emis file
	echo "Copying mcip file to scratch"
	cp ${MCIPDIR}/${SJDATE}/* ${SCRATCHDIR}/data/mcip3/  || exit 1 #copy mcip directory

	#COMPUTE VOLATILIZATION FUNCTION
	if [ $VOLATILIZATION ] ; then	
		echo "Computing volatilization"	
		Rscript ${VOLSCRIPTDIR}/create-vol-file.r ${SCRATCHDIR}/volatilization/vol.${STDATE}${number}.nc $SCRIPTDIR #create file where computed results are saved
		Rscript ${VOLSCRIPTDIR}/volatilization.r ${SCRATCHDIR}/volatilization/vol.${STDATE}${number}.nc $VOLSCRIPTDIR ${SCRATCHDIR}/data/mcip3/${METFILE} ${SCRATCHDIR}/init/${INITSFOC} ${SOILSAVEDIR}/init-soil${number}.nc ${SCRATCHDIR}/data/mcip3/${WATERFILE} #compute volatilization
		Rscript ${VOLSCRIPTDIR}/update-emis.r ${SCRATCHDIR}/volatilization/vol.${STDATE}${number}.nc ${SCRATCHDIR}/data/emis/${EMISNAME}_${SJDATE} #input computed values to emission file
	fi	

	if [ $x -eq 0 ] ; then 	#.....FIRST DAY SIMULATION.....#

		cd $HOMECMAQDIR/scripts/bcon/
		./run.bcon.multi

		#STDATE=`date -d "${FIRSTDATE}" +%Y%j` #used by jproc and cctm
		export ENDATE=`date -d "${FIRSTDATE} + ${END} day" +%Y%j` #used by jproc
		cd $HOMECMAQDIR/scripts/jproc/
		export M3DATAORIG=${HOMECMAQDIR}/data #jproc have problem to read data saved on scratch
		./run.jproc.multi	
		
		if [ -n "$Cdays" ] ; then
			export FIRST=0 #use CGRID from previous simulations
		else
			export FIRST=1 #TRUE

			cd $HOMECMAQDIR/scripts/icon/		
			./run.icon.multi
		fi
	else				#.....NEXT DAYS SIMULATIONS.....#
		export FIRST=0 #FALSE

		JDAY=$(( $x - 1 )) #previous day
		export DMD=`date -d "${FIRSTDATE} + ${JDAY} day" +%Y%j`			
		#echo $DMD	
	fi

	#echo $SJDATE
	#echo $STDATE
	echo "Running simulation"
	cd $HOMECMAQDIR/scripts/cctm/
	./run.cctm.multi	
	
	#REMOVE NOT NEEDED DATA
	echo "Copying data from scratch and removing useless data before next simulation day"
	rm ${SCRATCHDIR}/data/emis/${EMISNAME}_${SJDATE} #remove emis file
	rm -r ${SCRATCHDIR}/data/mcip3/* #remove mcip data
	#CGRID FILE - needed for next day
	rm -f ${SCRATCHDIR}/data/cgrid/* #remove previous cgrid file
	cp ${SCRATCHDIR}/data/cctm/${CGRIDNAME}.${STDATE}.ncf ${SCRATCHDIR}/data/cgrid/ #save for next day simulation

	#MOVE OUTPUTS
	#mv ${SCRATCHDIR}/cctm/* ${DATADIR}/data/cctm/
	cd ${SCRATCHDIR}/data/cctm/
	cp ${SCRATCHDIR}/data/cctm/${CGRIDNAME}.${STDATE}.ncf ${CCTMSAVEDIR}/${CGRIDNAME}.${STDATE}${number}.ncf #copy only CGRID (used for simulation recovery)
	#for f in ./*; do mv $f ${CCTMSAVEDIR}/${f%.ncf}${number}.ncf; done #add number of simulation at the end
	#SELECT AND SAVE IMPORTANT VARIABLE
	Rscript ${SCRIPTDIR}/extract-acon.r ${SCRATCHDIR}/data/cctm/${ACONNAME}.${STDATE}.ncf ${CCTMSAVEDIRR}/${ACONNAME}.${STDATE}${number}.ncf $SCRIPTDIR
	rm -r ${SCRATCHDIR}/data/cctm/* #delete all data in cctm directory

done

echo "Copying volatilization files from scratch"
if [ $VOLATILIZATION ] ; then
        mv ${SCRATCHDIR}/volatilization/* ${VOLSAVEDIR}/ #it's small file
fi
