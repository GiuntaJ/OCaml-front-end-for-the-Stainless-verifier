import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub7 {
  sealed case class Error(param0: String) extends Exception {}
  
  
  
  def sigma(((a, b, f))) = {
    val _2 = {
      def gen(a, b) = {
        
          if (
            a > b
          ) {
            assert(false, "Error with invalid arg ") 
          } else if (
            a == b
          ) {
            List(a) 
          } else {
            a :: gen(a + 1, b)
          }
      }
      val _3 = {
        def fsum(f, l, sum) = {
          l match {
            case Nil() => { sum }
            case Cons(h, t) => { fsum(f, t, sum) + f(h) }
          }
        }
        fsum(f, gen(a, b), 0)
      }
    }
  }
}
