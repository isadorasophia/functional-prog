#!/bin/sh

# check the corretude of a given program.

program="./findpath"
download_dir="answer"
output_dir="output"

mkdir ${output_dir}

# set -x # echo on

for i in $(ls ${download_dir} | grep .in)
do
    echo "reading file... ${download_dir}/${i}"
    swipl -q -f prog1.pl -g main -t halt < ${download_dir}/${i} > ${output_dir}/${i%%.*}.out

    echo "checking output!"
    DIFF=$(diff ${download_dir}/${i%%.*}.res ${output_dir}/${i%%.*}.out)

    if [ "$DIFF" != "" ]
    then
        echo "*** files are different! ***"
    else
        echo "done!"
        echo ""
    fi
done

