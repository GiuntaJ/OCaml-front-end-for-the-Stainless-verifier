import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub123 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { assert(false, "Failure with input list is empty") }
          case _ => {
            List.fold_left(
              {
                case (a, b) => { if (a > b) a else b }
              },
              lst.head, lst)
          }
        }
    }
  )
   
}