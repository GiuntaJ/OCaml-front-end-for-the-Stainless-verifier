import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub259 {
  /* 
  2011-10634
  JooHyun Jo / Major in Economics
  problem 3 for HW1
  */
  
  def iter(((n: Int63, f: A => A))): A => A = {
    n match {
      case 0 => { ( (x) => { x } ) }
      case _ => { ( (x) => { iter(n - 1, f, f(x)) } ) }
    }
  } 
    
}