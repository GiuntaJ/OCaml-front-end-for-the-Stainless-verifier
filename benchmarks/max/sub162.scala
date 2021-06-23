import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub162 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Cons(h, Nil()) => { h }
          case Cons(h, t) => { find(h, t) }
        }
    }
  )
  def find(x: Int63, y: List[Int63]): Int63 = {
    y match {
      case Nil() => { x }
      case Cons(h, t) => { if (x <= h) find(h, t) else find(x, t) }
    }
  }
   
}