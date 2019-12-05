#!/bin/bash

# TODO fix relative paths in this script to more robust solution 

# Link emacs config here
if [ ! -f $HOME/.emacs.d/init.el  ]; then
    ln ./.emacs.d/init.el ~/.emacs.d/init.el
    # TODO add option to replace existing emacs init file 
fi


# TODO automate installing emacs config file

# Download Luke Smith vim config to Download folder, link vim config
cd $HOME/Downloads
if [ ! -d voidrice ]; then
    git clone https://github.com/LukeSmithxyz/voidrice.git
fi

# If appropriate file found, link to user profile
if [ -f voidrice/.config/nvim/init.vim ]; then
    if [ ! -f $HOME/.vimrc  ]; then
	ln voidrice/.config/nvim/init.vim ~/.vimrc
    fi
fi
