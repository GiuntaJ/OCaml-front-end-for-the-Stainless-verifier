import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub10 {
  sealed case class Error(param0: String) extends Exception {}
  
  /* (+) -> adder */
  def adder(a: Int63, b: Int63): Int63 = { a + b }
  
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (x, y, f) =>
      {
        val _2 = {
          val sum = List.fold_left(adder, 0)
          val _3 = {
            def range(a, b) = { if (a > b) Nil() else a :: range(a + 1, b) }
            
              if (
                x > y
              ) {
                assert(false, "Error with Invalid arg ") 
              } else {
                sum(range(x, y).map(f))
              }
          }
        }
    }
  }
}