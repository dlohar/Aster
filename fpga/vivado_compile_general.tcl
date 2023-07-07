
# script for compiling with vivado
# parameters:
# $0: project name
# $1: function name
# $2: cpp file
# to run this script: vivado_hls vivado_compile_general.tcl $0 $1 $2

set project [lindex $argv 0]
set fncName [lindex $argv 1]
set file [lindex $argv 2]

# puts stdout ">>>>>> parameter:"
# puts stdout $project
# puts stdout $fncName
# puts stdout $file

# Commands to compile code to FPGA
open_project -reset $project
set_top $fncName
add_files $file
#add_files -tb $src_dir/mmult_test.cpp   #for simulation
open_solution -reset "solution"
set_part {xc7z020clg484-1}
create_clock -period 10 -name default
#csim_design -clean
csynth_design
close_project
exit