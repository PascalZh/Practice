#!/bin/bash

if [ -z $1 ];then
	echo 'Null project name!'
elif [ -d $1 ];then
	echo 'Folder already exists!'
else

# begin to create

mkdir -p $1/main

cd $1
touch Makefile
echo "#
# This is a project Makefile. It is assumed the directory this Makefile resides in is a
# project subdirectory.
#

PROJECT_NAME := $1

include \$(IDF_PATH)/make/project.mk
" >> Makefile
cd main
touch "$1_main.cpp"
vim "$1_main.cpp"

# end
fi
