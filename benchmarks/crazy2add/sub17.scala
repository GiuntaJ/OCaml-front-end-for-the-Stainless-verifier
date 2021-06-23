import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub17 {
  
  /*
   * Student no. : 2009-20769
   * Name        : Kim, Seongjun
   */
  
  /* 6.ml */
  sealed case class Error(param0: String) extends Exception {}
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((x, y))) = {
    val _2 = {
      def trim_zero(a) = { if (a == ZERO(NIL)) ZERO(NIL) else ZERO(a) }
      val _3 = {
        def c2a(((x, y))) = {
          (x, y) match {
            case (_, NIL) => { x }
            case (NIL, _) => { y }
            case (ZERO(xs), ONE(ys)) | (ONE(xs), ZERO(ys)) => { ONE(c2a(xs, ys))
            }
            case (ZERO(xs), MONE(ys)) | (MONE(xs), ZERO(ys)) => {
              MONE(c2a(xs, ys))
            }
            case (ONE(xs), ONE(ys)) => { trim_zero(c2a(c2a(xs, ONE(NIL)), ys)) }
            case (MONE(xs), MONE(ys)) => {
              trim_zero(c2a(c2a(xs, MONE(NIL)), ys))
            }
            case (ONE(xs), MONE(ys)) | (MONE(xs), ONE(ys)) |
            (ZERO(xs), ZERO(ys)) => {
              trim_zero(c2a(xs, ys))
            }
          }
        }
        (x, y) match {
          case (_, NIL) | (NIL, _) => {
            assert(false, "Error with NIL is not valid")
          }
          case (_, _) => { c2a(x, y) }
        }
      }
    }
  }
}