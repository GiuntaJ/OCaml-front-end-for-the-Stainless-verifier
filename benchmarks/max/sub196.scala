import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub196 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { failwith("None") }
          case Cons(h, t) => {
            val _2 = {
              def helper(((seen, rest))) = {
                rest match {
                  case Nil() => { seen }
                  case Cons(h_0, t_0) => {
                    val _5 = {
                      val seen_0 = if (h_0 > seen) h_0 else seen
                      val _6 = {
                        val rest_0 = t_0
                        helper(seen_0, rest_0)
                      }
                    }
                  }
                }
              }
              helper(h, t)
            }
          }
        }
    }
  ) 
   
}