set proj_path [lindex [glob ./work/*.xpr] 0]
# remove the .xpr extension and prefix paths
set proj_name [string range $proj_path 7 end-4]

open_project $proj_path

set_param general.maxThreads 8
reset_run impl_1 -prev_step
launch_runs impl_1 -jobs 10

wait_on_run impl_1
open_run impl_1

write_bitstream -force ${proj_name}.bit
