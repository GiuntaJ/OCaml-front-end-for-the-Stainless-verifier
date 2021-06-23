import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub61 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((left: Crazy2, right: Crazy2))): Crazy2 = {
    (left, right) match {
      case (_, NIL) => { left }
      case (NIL, _) => { right }
      case (ZERO(l0), ONE(r0)) | (ONE(l0), ZERO(r0)) => { ONE(crazy2add(l0, r0))
      }
      case (MONE(l0), ONE(r0)) | (ZERO(l0), ZERO(r0)) | (ONE(l0), MONE(r0)) => {
        ZERO(crazy2add(l0, r0))
      }
      case (ZERO(l0), MONE(r0)) | (MONE(l0), ZERO(r0)) => {
        MONE(crazy2add(l0, r0))
      }
      case (ONE(l0), ONE(r0)) => { ZERO(crazy2add(crazy2add(l0, r0), ONE(NIL)))
      }
      case (MONE(l0), MONE(r0)) => {
        ZERO(crazy2add(crazy2add(l0, r0), MONE(NIL)))
      }
    }
  }
}