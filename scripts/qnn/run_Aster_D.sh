#!/bin/bash --posix

# This script runs Aster's setting D to quantize all networks and subsequently
# runs Xilinx to evaluate the generated code.

# generate output directory
if [ ! -d Aster-SettingD ]; then
 mkdir Aster-SettingD
fi

output_folder="Aster-SettingD"
input_file_path="testcases/qnn"
function_name="nn1"

declare -a benchmarks=(
  "SmallNN" \
  "TestNN" \
  "LyapunovNN" \
  "AcrobotNN" \
  "InvPendulumNN" \
  "MountainCarNN" \
  "SinglePendulumNN" \
  "DoublePendulumNonRobustNN" \
  "DoublePendulumRobustNN" \
  "MpcNN" \
  "UnicycleNN" \
  "ACC3NN" \
  "ACC5NN" \
  "AirplaneNN" \
  "ACC7NN" \
  "ControllerToraNN" \
  "ACC10NN" \
  "VertCasNN" \
  "AC6NN" \
  "AC7NN" \
  "AC8NN"
)

declare -a layers=(2 2 2 3 2 3 3 3 3 3 2 4 6 4 8 4 11 6 4 4 4)
i=0

# make sure daisy is compiled
sbt compile
if [ ! -e daisy ]
then
 sbt script
fi

echo "... running Aster with setting D"
# Running benchmarks with their corresponding layers
for file in "${benchmarks[@]}"
do
 echo "****************************** Benchmark:" ${file} "******************************"
 timeout 300m ./daisy --qnn --qnn-quant --precision=Fixed32 --layer=${layers[$i]} --minLen=10 --maxLen=32 --initLen=17 --noInitError --lang=C --apfixed ${input_file_path}/${file}.scala

 mv model.lp ${output_folder}/${file}.lp

 # output gets generated in output/ folder with name objectName.cpp
 mv output/${file}.cpp ${output_folder}/
 timeout 300m vivado_hls scripts/qnn/vivado_compile_general.tcl ${file} ${function_name} ${output_folder}/${file}.cpp

 # the report is generated in folder object_name/solution/syn/report/${function_name}_csynth.rpt
 mv ${file}/solution/syn/report/${function_name}_csynth.rpt ${output_folder}/${file}_csynth.rpt

 # increase counter
 i=$(( i + 1 ))
done

