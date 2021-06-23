import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub151 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        val _4 = {
          def loop(fir, ilst) = {
            ilst match {
              case Nil() => { fir }
              case Cons(h, t) => { if (h >= loop(fir, t)) h else loop(fir, t) }
            }
          }
          lst match {
            case Cons(h, t) => { loop(h, t) }
            case Nil() => { 0 }
          }
        }
    }
  )
   
}