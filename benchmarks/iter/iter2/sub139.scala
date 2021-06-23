import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub139 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        (n, f) match {
          case (0, f) => { ( (x) => { x } ) }
          case (1, f) => { f }
          case (n, f) => { ( (x) => { f(iter(n - 1, f, x)) } ) }
        }
    }
  }
    
    iter(4, ( (x) => { 1 + x } ), 0)
  
  
  /*
  When n = 0, 
  iter(n, f) is defined to be the identity function. 
  When n > 0,
  iter(n, f) is the function 
  that applies f repeatedly n times. 
  
  For instance,
  iter(n, fun x -> 2+x) 0
  evaluates to 2 × n.
  */
}