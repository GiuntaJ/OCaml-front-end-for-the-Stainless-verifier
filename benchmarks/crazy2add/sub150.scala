import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub150 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((a, b))) = {
    (a, b) match {
      case (a, NIL) => { a }
      case (NIL, b) => { b }
      case (ZERO(a2), MONE(b2)) => { MONE(crazy2add(a2, b2)) }
      case (ZERO(a2), ZERO(b2)) => { ZERO(crazy2add(a2, b2)) }
      case (ZERO(a2), ONE(b2)) => { ONE(crazy2add(a2, b2)) }
      case (MONE(a2), ZERO(b2)) => { MONE(crazy2add(a2, b2)) }
      case (ONE(a2), ZERO(b2)) => { ONE(crazy2add(a2, b2)) }
      case (ONE(a2), ONE(b2)) => { ZERO(crazy2add(crazy2add(ONE(NIL), a2), b2))
      }
      case (MONE(a2), MONE(b2)) => {
        ZERO(crazy2add(crazy2add(MONE(NIL), a2), b2))
      }
      case (ONE(a2), MONE(b2)) => { ZERO(crazy2add(a2, b2)) }
      case (MONE(a2), ONE(b2)) => { ZERO(crazy2add(a2, b2)) }
    }
  }
}
