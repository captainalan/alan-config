#!/bin/bash

THIS_DIR=`pwd`

# Link emacs config here
if [ ! -f $HOME/.emacs.d/init.el  ]; then
	echo "Setting up emacs stuff"
	ln $THIS_DIR/.emacs.d/init.el ~/.emacs.d/init.el
	if [ ! -d $HOME/.emacs.d/snippets ]; then
		ln -s $THIS_DIR/.emacs.d/snippets ~/.emacs.d/
	fi
	# Add codes to automatically update packages, download things
fi

# Link custom lisp code
if [ ! -d $HOME/.emacs.d/lisp ]; then
	echo "Linking lisp directory"
	ln -s $THIS_DIR/.emacs.d/lisp ~/.emacs.d/
fi

echo "done"
exit 0;

