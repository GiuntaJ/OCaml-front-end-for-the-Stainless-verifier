import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub186 {
  /*print_endline "p3"*/
  
  def iter(((n: Int63, f: Int63 => Int63))): Int63 => Int63 = {
    
      if (
        n < 0
      ) {
        ( (x) => { x } ) 
      } else if (
        n == 0
      ) {
        ( (x) => { x } ) 
      } else {
        ( (x) => { iter(n - 1, f, f(x)) } )
      }
  }
  
  /*;; 10 |> iter (3, (fun x -> x + 1)) |> print_int*/
  /*; print_endline ""*/
  /*;; 102 |> iter (2, (fun x -> -x)) |> print_int*/
  /*; print_endline ""*/
}