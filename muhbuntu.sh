#!/bin/bash

THIS_DIR=`pwd`

# TODO fix relative paths in this script to more robust solution
if [ ! -d $HOME/Repos ]; then
	mkdir $HOME/Repos
fi

# Link emacs config here
if [ ! -f $HOME/.emacs.d/init.el  ]; then
	echo "Gotta setup emacs"
	# ln ./.emacs.d/init.el ~/.emacs.d/init.el
	# TODO add option to replace existing emacs init file
fi


# TODO automate installing emacs config file

# Download Luke Smith vim config to Download folder, link vim config
cd $HOME/Repos
if [ ! -d voidrice ]; then
	git clone https://github.com/LukeSmithxyz/voidrice.git
fi

# If appropriate file found, link to user profile
if [ -f voidrice/.config/nvim/init.vim ]; then
	if [ ! -f $HOME/.vimrc  ]; then
		ln voidrice/.config/nvim/init.vim ~/.vimrc
	fi
fi

cd $THIS_DIR
if [ ! -f $HOME/.Xmodmap  ]; then
	ln ./.Xmodmap ~/.Xmodmap
fi

# Gotta add the following line to some init script to make this persist
xmodmap $HOME/.Xmodmap
