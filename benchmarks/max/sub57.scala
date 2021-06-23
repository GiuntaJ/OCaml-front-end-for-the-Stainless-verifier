import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub57 {
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Cons(x, Nil()) => { x }
          case Cons(h, t) => { if (h > max(t)) h else max(t) }
        }
    }
  )
  
   
}