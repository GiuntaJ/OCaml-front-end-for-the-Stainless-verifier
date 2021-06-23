import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub256 {
  def iter[A](pair: (Int63, (A => A))): A => A = {
    val _2 = {
      val ((n, f)) = pair
      
        if (
          n <= 0
        ) {
          ( (x) => { x } ) 
        } else {
          val _4 = {
            def compose(f, g, x) = { f(g(x)) }
            compose(f, iter(n - 1, f))
          }
        }
    }
  }
  
  /*
  let test (f : 'a -> 'b) (input : 'a) (output : 'b) : unit =
    if ((f input) = output)
      then ((print_string ("correct answer")); (print_newline ()))
      else ((print_string ("wrong answer")); (print_newline ()))
  
  let _ =
    let fu = iter (10, (fun x -> 2 + x) ) in
    print_int (fu 0); (print_newline())
  */
}