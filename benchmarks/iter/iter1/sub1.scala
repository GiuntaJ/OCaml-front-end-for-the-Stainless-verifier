import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub1 {
  sealed case class Error(param0: String) extends Exception {}
  
  def iter(((n, f))) = {
    val _2 = {
      def identity(x) = { x }
      val _3 = {
        def ff(x) = { f(f(x)) }
        
          if (
            n < 0
          ) {
            assert(false, "Error with Garbage In ") 
          } else {
            n match {
              case 0 => { identity }
              case 1 => { f }
              case _ => { iter(n - 1, ff) }
            }
          }
      }
    }
  }
}