import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub177 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        List.fold_left(
          {
            case (x, y) => { if (y > x) y else x }
          },
          lst.head, lst)
    }
  )
   
}