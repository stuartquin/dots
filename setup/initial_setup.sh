#!/bin/bash
sudo apt-get update

#Git
sudo apt-get -y --fix-missing install git
sudo apt-get -y --fix-missing install tig
git config --global status.displaycommentprefix true

# Basic Python Stuff
sudo apt-get -y --fix-missing install pip
sudo apt-get -y --fix-missing install python-pip
sudo pip install virtualenvwrapper

# Vim
sudo apt-get -y --fix-missing install vim-gnome
sudo apt-get -y --fix-missing install silversearcher-ag

# Useful things
sudo apt-get -y --fix-missing install htop
sudo apt-get -y --fix-missing install network-manager-openvpn
sudo apt-get -y --fix-missing install gnome-tweak-tool

# Some default directories
mkdir ~/.vimtmp
mkdir ~/Projects
mkdir ~/Apps

# Fancy ls
cd ~/Apps
git clone git://github.com/trapd00r/ls--.git
cd ls--
perl Makefile.PL
make
sudo make install
sudo cpan Term::ExtendedColor Term::ExtendedColor::Xresources
cpan Term::ExtendedColor

# Install zsh
sudo apt-get -y --fix-missing install zsh
chsh -s /bin/zsh

#Xmonad
sudo apt-get -y --fix-missing install xmonad gnome gmrun

ln -s ~/Dropbox/vimwiki
