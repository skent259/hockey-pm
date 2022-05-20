#!/bin/bash

# start from ~/condor

## run_shots-ppool.sub -----------------------------------#

# TODO: convert these logs to model/log
cd logs/shots-ppool

rm mi-bl/*.err
rm mi-bl/*.log
rm mi-bl/*.out
rm sh-go/*.err
rm sh-go/*.log
rm sh-go/*.out

cd ../..

## run_shots-ppool_nt.sub -----------------------------------#

# TODO: convert these logs to model/log
cd logs/shots-ppool-nt

rm mi-bl/*.err
rm mi-bl/*.log
rm mi-bl/*.out
rm sh-go/*.err
rm sh-go/*.log
rm sh-go/*.out

cd ../..


## run_cv-shots-ppool.sub --------------------------------#

cd logs/cv-shots-ppool

rm err/*
rm log/*
rm out/*

cd ../..