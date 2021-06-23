import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub152 {
  /********************/
  /*     Problem 1     */
  /********************/  
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        List.fold_right(
          {
            case (x, y) => { if (x > y) x else y }
          },
          lst, lst.head)
    }
  )
   
}