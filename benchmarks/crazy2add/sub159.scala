import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub159 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((a: Crazy2, b: Crazy2))): Crazy2 = {
    (a, b) match {
      case (NIL, _) => { b }
      case (_, NIL) => { a }
      case (ZERO(a_pre), ZERO(b_pre)) => { ZERO(crazy2add(a_pre, b_pre)) }
      case (ZERO(a_pre), ONE(b_pre)) => { ONE(crazy2add(a_pre, b_pre)) }
      case (ONE(a_pre), ZERO(b_pre)) => { ONE(crazy2add(a_pre, b_pre)) }
      case (ZERO(a_pre), MONE(b_pre)) => { MONE(crazy2add(a_pre, b_pre)) }
      case (MONE(a_pre), ZERO(b_pre)) => { MONE(crazy2add(a_pre, b_pre)) }
      case (ONE(a_pre), MONE(b_pre)) => { ZERO(crazy2add(a_pre, b_pre)) }
      case (MONE(a_pre), ONE(b_pre)) => { ZERO(crazy2add(a_pre, b_pre)) }
      case (ONE(a_pre), ONE(b_pre)) => {
        crazy2add(ZERO(crazy2add(a_pre, b_pre)), ZERO(ONE(NIL)))
      }
      case (MONE(a_pre), MONE(b_pre)) => {
        crazy2add(ZERO(crazy2add(a_pre, b_pre)), ZERO(MONE(NIL)))
      }
    }
  }
}
