import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub8 {
  
  sealed case class Error(param0: String) extends Exception {}
  	
  def iter(((n, f)), init) = {
    val _2 = {
      def gen(n) = {
        
          if (
            n == 0
          ) {
            Nil() 
          } else if (
            n < 0
          ) {
            assert(false, "Error with invalid arg ") 
          } else {
            1 :: gen(n - 1)
          }
      }
      val _3 = {
        def iter_sub(f, iter_list, result) = {
          iter_list match {
            case Nil() => { result }
            case Cons(h, t) => { iter_sub(f, t, f(result)) }
          }
        }
        iter_sub(f, gen(n), init)
      }
    }
  }
}