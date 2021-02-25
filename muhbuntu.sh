#!/bin/bash

THIS_DIR=`pwd`

# TODO fix relative paths in this script to more robust solution
if [ ! -d $HOME/Repos ]; then
	mkdir $HOME/Repos
fi

# Link emacs config here
if [ ! -f $HOME/.emacs.d/init.el  ]; then
	echo "Setting up emacs stuff"
	ln $THIS_DIR/.emacs.d/init.el ~/.emacs.d/init.el
	if [ ! -d $HOME/.emacs.d/snippets ]; then
		ln -s $THIS_DIR/.emacs.d/snippets ~/.emacs.d/
	fi
	if [ ! -d $HOME/.emacs.d/lisp ]; then
		ln -s $THIS_DIR/.emacs.d/lisp ~/.emacs.d/
	fi
	# Add codes to automatically update packages, download things
fi

# TODO automate installing emacs config file
