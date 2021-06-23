import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub111 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((icr1: Crazy2, icr2: Crazy2))): Crazy2 = {
    (icr1, icr2) match {
      case (NIL, NIL) => { NIL }
      case (ZERO(cr1), ZERO(cr2)) => { ZERO(crazy2add(cr1, cr2)) }
      case (ZERO(cr1), ONE(cr2)) => { ONE(crazy2add(cr1, cr2)) }
      case (ZERO(cr1), MONE(cr2)) => { MONE(crazy2add(cr1, cr2)) }
      case (ZERO(cr1), NIL) => { ZERO(crazy2add(cr1, NIL)) }
      case (ONE(cr1), ZERO(cr2)) => { ONE(crazy2add(cr1, cr2)) }
      case (ONE(cr1), ONE(cr2)) => {
        ZERO(crazy2add(crazy2add(cr1, cr2), ONE(NIL)))
      }
      case (ONE(cr1), MONE(cr2)) => { ZERO(crazy2add(cr1, cr2)) }
      case (ONE(cr1), NIL) => { ONE(crazy2add(cr1, NIL)) }
      case (MONE(cr1), ZERO(cr2)) => { MONE(crazy2add(cr1, cr2)) }
      case (MONE(cr1), ONE(cr2)) => { ZERO(crazy2add(cr1, cr2)) }
      case (MONE(cr1), MONE(cr2)) => {
        ZERO(crazy2add(crazy2add(cr1, cr2), MONE(NIL)))
      }
      case (MONE(cr1), NIL) => { MONE(crazy2add(cr1, NIL)) }
      case (NIL, ZERO(cr2)) => { ZERO(crazy2add(cr2, NIL)) }
      case (NIL, ONE(cr2)) => { ONE(crazy2add(cr2, NIL)) }
      case (NIL, MONE(cr2)) => { MONE(crazy2add(cr2, NIL)) }
    }
  }
}
