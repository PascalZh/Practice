#!/bin/bash

if [ -z $1 ];then
	let i=45
else
	if [ $1 -gt 0 ];then
		let i=$1
	else
		echo "Arguments not correct!"
		exit
	fi
fi
echo 'env DISPLAY=:0 feh -F ~/Downloads/tomato.png' | at now+$i minutes
