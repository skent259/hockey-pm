#!/bin/bash

mkdir .R
mkdir packages
export HOME=$PWD
# module avail
module load GCC/8.3.0

mv Makevars .R/

tar -xzf R412.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R412
export R_LIBS=$PWD/packages
R --version

# # Run these lines by hand, within R
# Rscript install-r-packages.R

tar -czf packages.tar.gz packages/ .R/