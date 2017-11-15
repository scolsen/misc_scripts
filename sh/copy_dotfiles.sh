#!/bin/bash
# Simple script to copy common dotfiles from their usual locations to the target location provided as an argument.
cp $HOME/.bashrc $1/.bashrc
echo "Copied .bashrc to $1/.bashrc" 
cp $HOME/.screenrc $1/.screenrc
echo "Copied .screenrc to $1/.screenrc"
cp $HOME/.vimrc $1/.vimrc
echo "Copied .vimrc to $1/.vimrc"
cp /usr/local/etc/lynx.cfg $1/lynx.cfg
echo "Copied lynx.cfg to $1/lynx.cfg"
cp /usr/local/etc/lynx.lss $1/lynx.lss
echo "Copied lynx.lss to $1/lynx.lss"
cp $HOME/.tmux.conf $1/.tmux.conf
echo "Copied tmux.conf to $1/.tmux.conf"
