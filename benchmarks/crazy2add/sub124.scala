import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub124 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    (c1, c2) match {
      case (ONE(x), ONE(y)) => {
        crazy2add(ZERO(ONE(NIL)), ZERO(crazy2add(x, y)))
      }
      case (MONE(x), MONE(y)) => {
        crazy2add(ZERO(MONE(NIL)), ZERO(crazy2add(x, y)))
      }
      case (MONE(x), ONE(y)) => { ZERO(crazy2add(x, y)) }
      case (ONE(x), MONE(y)) => { ZERO(crazy2add(x, y)) }
      case (NIL, _) => { c2 }
      case (_, NIL) => { c1 }
      case (ZERO(x), ONE(y)) => { ONE(crazy2add(x, y)) }
      case (ZERO(x), MONE(y)) => { MONE(crazy2add(x, y)) }
      case (ONE(x), ZERO(y)) => { ONE(crazy2add(x, y)) }
      case (MONE(x), ZERO(y)) => { MONE(crazy2add(x, y)) }
      case (ZERO(x), ZERO(y)) => { ZERO(crazy2add(x, y)) }
    }
  }
}