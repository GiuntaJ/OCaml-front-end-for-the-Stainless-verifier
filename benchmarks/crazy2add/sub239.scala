import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub239 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((a, b))) = {
    (a, b) match {
      case (_, NIL) => { a }
      case (NIL, _) => { b }
      case (ZERO(a_0), ZERO(b_0)) => { ZERO(crazy2add(a_0, b_0)) }
      case (ZERO(a_0), ONE(b_0)) => { ONE(crazy2add(a_0, b_0)) }
      case (ZERO(a_0), MONE(b_0)) => { MONE(crazy2add(a_0, b_0)) }
      case (ONE(a_0), ZERO(b_0)) => { ONE(crazy2add(a_0, b_0)) }
      case (ONE(a_0), ONE(b_0)) => {
        ZERO(crazy2add(crazy2add(a_0, b_0), ONE(NIL)))
      }
      case (ONE(a_0), MONE(b_0)) => { ZERO(crazy2add(a_0, b_0)) }
      case (MONE(a_0), ZERO(b_0)) => { MONE(crazy2add(a_0, b_0)) }
      case (MONE(a_0), ONE(b_0)) => { ZERO(crazy2add(a_0, b_0)) }
      case (MONE(a_0), MONE(b_0)) => {
        ZERO(crazy2add(crazy2add(a_0, b_0), MONE(NIL)))
      }
    }
  }
}