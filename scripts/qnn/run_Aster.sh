#!/bin/bash --posix

# This script runs Daisy to quantize a neural network

input_file=$1   # full path to input file
function_name=$2
n=$3 #number of layers
setting=$4

prefix="testcases/qnn/"
suffix=".scala"
foo=${input_file#"$prefix"}
object_name=${foo%"$suffix"}

# generate output directory
if [ ! -d Aster-Single-Network ]; then
 mkdir Aster-Single-Network
fi

output_folder="Aster-Single-Network"

# make sure daisy is compiled
sbt compile
if [ ! -e daisy ]
then
  sbt script
fi

# Running benchmark
echo "****************************** Benchmark:" ${object_name} "******************************"
if [ ${setting} == "A" ]
then
 timeout 300m ./daisy --qnn --qnn-quant --precision=Fixed32 --layer=${n} --minLen=5 --maxLen=32 --initLen=20 --lang=C --apfixed ${input_file}
elif [ ${setting} == "B" ]
then
 timeout 300m ./daisy --qnn --qnn-quant --precision=Fixed32 --layer=${n} --minLen=10 --maxLen=32 --initLen=32 --lang=C --apfixed ${input_file}
elif [ ${setting} == "C" ]
then
 timeout 300m ./daisy --qnn --qnn-quant --precision=Fixed32 --layer=${n} --minLen=5 --maxLen=32 --initLen=10 --noInitError --lang=C --apfixed ${input_file}
elif [ ${setting} == "D" ]
then
 timeout 300m ./daisy --qnn --qnn-quant --precision=Fixed32 --layer=${n} --minLen=10 --maxLen=32 --initLen=17 --noInitError --lang=C --apfixed ${input_file}
else
 echo "Error! Please provide a correct settings!! Cannot continue!!"
fi
mv model.lp ${output_folder}/${object_name}.lp

# output gets generated in output/ folder with name objectName.cpp
mv output/${object_name}.cpp ${output_folder}/${object_name}.cpp
