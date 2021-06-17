# OCaml-front-end-for-the-Stainless-verifier
## Master semester project at LARA, EPFL

The main idea of the project was to implement a front end to translate OCaml programs in Scala, in such a way that it can be used within Stainless.
For a better overview of the project, you can check the PDF report at the root of this repository.

For more informations about Stainless : <https://stainless.epfl.ch/>

### Usage

To translate your OCaml programs, you need to: 
  * Install Camlp5 (see <https://github.com/camlp5/camlp5>).
  * Install OCaml so that you can use `ocamlc`.
  * Install Scala so that you can use `scalac`.
  * Run the shell script with the command: 
   ```$ sh ocaml_to_scala.sh path/to/the/file/to/translate.ml```
