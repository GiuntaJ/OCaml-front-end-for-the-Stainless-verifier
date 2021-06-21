#!/bin/bash
# A shell script to transform OCaml programs to Scala programs.
# The first argument is the name of the OCaml file to transform.
OCAML_INPUT_FILE="${1}"
FILE_NAME="${1/.ml/}"
OCAML_INTERFACE_FILE="interface.txt"
OCAML_INPUT_WITH_TYPES="${FILE_NAME}_with_types.ml"
SCALA_OUTPUT_FILE="${FILE_NAME}.scala"

# Compile the OCaml file and retrieve the interface.
echo "Compiling the input file to retrieve the interface..."
ocamlc -i $OCAML_INPUT_FILE >$OCAML_INTERFACE_FILE

# Insert the types from the interface in the OCaml program.
echo "Inserting the retrieved types in the input file..."
scalac merge_interface_implementation.scala
scala OCamlInterfaceMerger \
    $OCAML_INTERFACE_FILE $OCAML_INPUT_FILE $OCAML_INPUT_WITH_TYPES

# Translate the OCaml program in Scala.
echo "Translating to Scala..."
ocamlfind ocamlc -syntax camlp5r -package camlp5 -linkpkg pr_scala.ml
camlp5o pr_o.cmo ./pr_scala.cmo $OCAML_INPUT_WITH_TYPES >$SCALA_OUTPUT_FILE

# Wrap the Scala program in an object and add the import statements.
SCALA_OBJECT=${FILE_NAME////_}
TO_PREPEND="import stainless.collection._\\
import stainless.io.StdOut._\\
import stainless.lang._\\
import stainless.math._\\
import stainless.math.BitVectors._\\
\\
object $SCALA_OBJECT {\n"
sed -i -e 's/^/  /' $SCALA_OUTPUT_FILE
sed -i -e "1s;^;$TO_PREPEND;" $SCALA_OUTPUT_FILE
sed -i -e '$s;$;\
};' $SCALA_OUTPUT_FILE

# Remove unnecessary files.
rm OCamlInterfaceMerger.class
rm OCamlInterfaceMerger$.class
rm pr_scala.cmo
rm pr_scala.cmi
rm a.out
rm $OCAML_INTERFACE_FILE
rm $OCAML_INPUT_WITH_TYPES
