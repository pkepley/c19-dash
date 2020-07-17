#!/bin/bash

NOW="$(date)"
JHUGITDIR="$HOME/git-repositories/covid-19"
SHINYDIR="$HOME/shiny-apps/covid-19"

echo "The current time is $NOW" 2>&1 | tee -a $HOME/covid_pull.log
cd $JHUGITDIR
git pull 2>&1 | tee -a $HOME/covid_pull.log
cd $SHINYDIR

sudo swapon -v /swapfile
R -f prep_data.R
sudo swapoff -v /swapfile
