#!/bin/bash
# This script needs to be run from the same directory level than ocaml_to_scala.sh

for FILE in benchmarks/{crazy2add,\
diff/diff1,diff/diff2,\
filter,\
formula/formula1,formula/formula2,\
iter/iter1,iter/iter2,\
lambda/lambda1,lambda/lambda2,lambda/lambda3,\
max,\
mem,\
mirror,\
sigma/sigma1,sigma/sigma2}/*.ml;
do
    echo $FILE
    sh ocaml_to_scala.sh $FILE
done
