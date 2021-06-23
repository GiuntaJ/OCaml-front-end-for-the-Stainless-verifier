import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub123 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        n match {
          case 0 => { f }
          case _ => { iter(n - 1, ( (f) => { f } )) }
        }
    }
  }
  
      
      
  iter(1, ( (x) => { 2 + x } ))
}