#!/bin/bash
echo "########### Translate files ###########"
for FILE in test/input/*.ml; do
    echo $FILE
    sh ocaml_to_scala.sh $FILE
done

echo "########### Scala Tests ###########"
for FILE in test/input/*.scala; do
    echo $FILE
    scalac $(
        find \
        /Users/julie/Documents/Repos/stainless/frontends/library/stainless \
        -name "*.scala"
    ) $FILE
done

echo "########### Stainless Tests ###########"
for FILE in test/input/*.scala; do
    echo $FILE
    stainless $FILE
done
