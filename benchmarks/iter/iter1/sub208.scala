import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub208 {
  /*print_endline "p3"*/
  
  def iter(((n: Int63, f: A => A))): A => A = {
    
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
  
  /*let _ =*/
      /*let assert_equal_i (expected: int) (actual: int) =*/
          /*if expected = actual then print_endline "true"*/
          /*else Printf.printf "Expected %d but actual %d
  " expected actual*/
      /*in*/
      /*let assert_equal_f (expected: float) (actual: float) =*/
          /*if expected = actual then print_endline "true"*/
          /*else Printf.printf "Expected %f but actual %f
  " expected actual*/
      /*in*/
      /*let test_iter_i (n: int) (f: int->int) (arg: int) (expected: int) =*/
          /*arg |> iter (n, f) |> assert_equal_i expected*/
      /*in*/
      /*let test_iter_f (n: int) (f: float->float) (arg: float) (expected: float) =*/
          /*arg |> iter (n, f) |> assert_equal_f expected*/
      /*in*/
      /*test_iter_i 3 (fun x -> x + 1) 10 13;*/
      /*test_iter_i 10 (fun x -> x * 2) 1 1024;*/
      /*test_iter_f 3 (fun x -> x +. 1.0) 10.0 13.0;*/
}