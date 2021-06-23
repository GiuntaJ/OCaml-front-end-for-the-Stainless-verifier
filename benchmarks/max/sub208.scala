import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub208 {
  /********************/
  /*     Problem 1     */
  /********************/
  
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { min_int }
          case Cons(h, t) => {
            val _2 = {
              val s = max(t)
              if (s > h) s else h
            }
          }
        }
    }
  )
   
}