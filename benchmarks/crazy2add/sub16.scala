import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub16 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  def crazy2add(((x, y))) = {
    (x, y) match {
      case (_, NIL) => { x }
      case (NIL, _) => { y }
      case (ONE(xs), ONE(ys)) => { ZERO(crazy2add(crazy2add(xs, ONE(NIL)), ys))
      }
      case (MONE(xs), MONE(ys)) => {
        ZERO(crazy2add(crazy2add(xs, MONE(NIL)), ys))
      }
      case (ZERO(xs), ONE(ys)) | (ONE(xs), ZERO(ys)) => { ONE(crazy2add(xs, ys))
      }
      case (ZERO(xs), MONE(ys)) | (MONE(xs), ZERO(ys)) => {
        MONE(crazy2add(xs, ys))
      }
      case (ONE(xs), MONE(ys)) | (MONE(xs), ONE(ys)) | (ZERO(xs), ZERO(ys)) => {
        ZERO(crazy2add(xs, ys))
      }
    }
  }
}