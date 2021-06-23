import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub158 {
  def compose(f, g, x) = { f(g(x)) }
  /*val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> */
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n < 0
          ) {
            assert(false, "Failure with n is eumsu ") 
          } else if (
            n == 0
          ) {
            ( (x) => { x } ) 
          } else {
            compose(f, iter(n - 1, f))
          }
    }
  } /* (iter~) 이부분이 compose 함수의 g 부분*/
      
      
  /*Test Case*/
  /* iter(0, function x -> x + 2) 1 ;;*/
}