import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub52 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    (c1, c2) match {
      case (NIL, NIL) => { NIL }
      case (NIL, ZERO(c)) => { ZERO(c) }
      case (NIL, ONE(c)) => { ONE(c) }
      case (NIL, MONE(c)) => { MONE(c) }
      case (ZERO(c), NIL) => { ZERO(c) }
      case (ZERO(c), ZERO(r)) => { ZERO(crazy2add(c, r)) }
      case (ZERO(c), ONE(r)) => { ONE(crazy2add(c, r)) }
      case (ZERO(c), MONE(r)) => { MONE(crazy2add(c, r)) }
      case (ONE(c), NIL) => { ONE(c) }
      case (ONE(c), ZERO(r)) => { ONE(crazy2add(c, r)) }
      case (ONE(c), ONE(r)) => { ZERO(crazy2add(ONE(NIL), crazy2add(c, r))) }
      case (ONE(c), MONE(r)) => { ZERO(crazy2add(c, r)) }
      case (MONE(c), NIL) => { MONE(c) }
      case (MONE(c), ZERO(r)) => { MONE(crazy2add(c, r)) }
      case (MONE(c), ONE(r)) => { ZERO(crazy2add(c, r)) }
      case (MONE(c), MONE(r)) => { ZERO(crazy2add(MONE(NIL), crazy2add(c, r))) }
    }
  }
}