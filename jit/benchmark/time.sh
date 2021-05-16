#!/bin/bash

#Simplify programs and prepare them for interpretation
cd ../
racket eval-benchmarks.rkt
cd benchmark

cd original

echo "***********************************************Running partially evaluated programs*************************************************"
echo 

for FILE in *; do 
	echo
	echo "--------------------------------------------${FILE}---------------------------------------------------------"

	time racket "../simplified/${FILE}"; 
done

echo
echo "**************************************************Interpreting programs*************************************************************"

for FILE in *; do 
        echo
        echo "--------------------------------------------${FILE}---------------------------------------------------------"

        time racket "../interp/${FILE}"; 
done


echo
echo "*************************************************Running compiled programs************************************************************"

for FILE in *; do
	echo
	echo "-------------------------------------------${FILE}-------------------------------------------------------------"

	cd ../../
	make "./benchmark/compile/${FILE}.run"
	chmod 755 "./benchmark/compile/${FILE}.run"
	cd benchmark/original
	time ../"compile/${FILE}.run" < ../compile/input
done
