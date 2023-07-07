#!/bin/bash --posix

# This script runs Daisy's mixed precision analysis for all networks and 
# subsequently runs Xilinx to evaluate the generated code

# generate output directory
if [ ! -d mixed-precision ]; then
 mkdir mixed-precision
fi

output_folder="mixed-precision"
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

# make sure daisy is compiled
sbt compile
if [ ! -e daisy ]
then
  sbt script
fi

for file in "${benchmarks[@]}"
do
  echo "****************************** Benchmark:" ${file} "******************************"
  # to add a timeout: /usr/bin/time -f "%e" timeout 10m
  timeout 300m ./daisy --qnn --codegen --lang=C --apfixed --mixed-tuning --precision=Fixed32 ${input_file_path}/${file}.scala

  # output gets generated in output/ folder with name objectName.cpp
  mv output/${file}.cpp ${output_folder}/

  timeout 300m vivado_hls scripts/qnn/vivado_compile_general.tcl ${file} ${function_name} ${output_folder}/${file}.cpp

  # the report is generated in folder object_name/solution/syn/report/${function_name}_csynth.rpt
  mv ${file}/solution/syn/report/${function_name}_csynth.rpt ${output_folder}/${file}_csynth.rpt

done
