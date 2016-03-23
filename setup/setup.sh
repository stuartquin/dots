#!/bin/bash

# List all the files in home directory
HOME_DIR=./home/.[A-Za-z]*

script_dir=`pwd`

echo $script_dir

for f in $HOME_DIR
do
  link=${f:7}
  ln -s $script_dir/$f ~/
  echo "ln -s $f ~/."
done
