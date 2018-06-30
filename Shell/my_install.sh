#!/bin/bash

dpkg -s zsh
if [ "$?" != 0  ];then
	sudo apt install zsh
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
fi

dpkg -s vim
if [ "$?" == 0 ];then
	sudo apt remove vim
fi
