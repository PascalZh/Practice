## Some of this shell scripts are my daily commands.
They are very useful.
You can create hard link in the bin fold to use them. For example:

ln ./tmt_clk ~/bin/tmt_clk

There are some specific explanation for some scripts:
1)	To use __create_esp_project.sh, you may add hard link in the bin fold.
But use the other name, such as:

ln ./__create_esp_project ~/bin/__create_esp_project_e

Then create alias in your shell:

alias __create_esp_project='source ~/bin/__create_esp_project_e'

For you have to use source to let your cd really work.
