#!/bin/bash

# R setup on CHTC
tar -xzf R412.tar.gz
tar -xzf packages.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages

# run script
outcome=$1
d_fname=$2

Rscript ppool_shots.R $outcome $d_fname