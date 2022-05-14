set proj_path [lindex [glob ./work/*.xpr] 0]

open_project $proj_path

write_project_tcl -force -target_proj_dir "./work" ./scripts/setup.tcl
