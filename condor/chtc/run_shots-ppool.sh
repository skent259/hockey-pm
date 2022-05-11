#!/bin/bash

export PATH
. /etc/profile.d/modules.sh
module load GCC/8.3.0

# R setup on CHTC
tar -xzf R412.tar.gz
tar -xzf packages.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages

export HOME=$PWD

# run script
outcome=$1
d_fname=$2

Rscript ppool_shots.R $outcome $d_fname