RTL = ${wildcard rtl/**/*.v rtl/**/*.vh}

all: setup compile

.PHONY : setup
setup : .setup.done

.setup.done : ./scripts/setup.tcl
	pwsh.exe /c "vivado -mode batch -source ./scripts/setup.tcl -log setup.log -jou setup.jou -tclargs --origin_dir ./scripts && New-Item -Force -ItemType File -Name .setup.done"

.PHONY : compile
compile : .compile.done

.compile.done : $(RTL)
	pwsh.exe /c "vivado -mode batch -source ../../scripts/compile.tcl -log compile.log -jou compile.jou && New-Item -Force -ItemType File -Name .compile.done"

.PHONY : generate_setup_tcl
generate_setup_tcl :
	pwsh.exe /c "vivado -mode batch -source ../../scripts/generate_setup_tcl.tcl"


.PHONY : open_project
open_project :
	pwsh.exe /c 'Start-Process -WindowStyle Hidden -FilePath "vivado" -ArgumentList "-mode gui ${wildcard work/*.xpr}"'

.PHONY : clean
clean:
	pwsh.exe /c "if (Test-Path .setup.done) {Remove-Item .setup.done}; if (Test-Path .compile.done) {Remove-Item .compile.done}; if (Test-Path work/) {Remove-Item -Recurse work/}"
