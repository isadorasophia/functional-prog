#!/bin/sh

website="https://susy.ic.unicamp.br:9999/mc346a/03/dados/"
download_dir="answer"

n_files=4

mkdir ${download_dir}
cd ${download_dir}

# start downloading from website
for i in $(seq 1 ${n_files})
do
	curl -o arq${i}.in ${website}arq${i}.in -k
	curl -o arq${i}.res ${website}arq${i}.res -k
done 
