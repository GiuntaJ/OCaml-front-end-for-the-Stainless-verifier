import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub76 {
  /*2-2 컴공 2014-10618 이세영*/
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  def addThree(((a, b, c))) = {
    (a, b, c) match {
      case (NIL, NIL, -1) => { MONE(NIL) }
      case (NIL, NIL, 0) => { NIL }
      case (NIL, NIL, 1) => { ONE(NIL) }
      case (NIL, MONE(x), -1) => { ZERO(addThree(x, NIL, -(1))) }
      case (NIL, MONE(x), 0) => { MONE(x) }
      case (NIL, MONE(x), 1) => { ZERO(x) }
      case (NIL, ZERO(x), -1) => { MONE(x) }
      case (NIL, ZERO(x), 0) => { ZERO(x) }
      case (NIL, ZERO(x), 1) => { ONE(x) }
      case (NIL, ONE(x), -1) => { ZERO(x) }
      case (NIL, ONE(x), 0) => { ONE(x) }
      case (NIL, ONE(x), 1) => { ZERO(addThree(x, NIL, 1)) }
      case (ONE(z), NIL, -1) => { ZERO(z) }
      case (ONE(z), NIL, 0) => { ONE(z) }
      case (ONE(z), NIL, 1) => { ZERO(addThree(z, NIL, 1)) }
      case (ONE(z), MONE(x), -1) => { MONE(addThree(z, x, 0)) }
      case (ONE(z), MONE(x), 0) => { ZERO(addThree(z, x, 0)) }
      case (ONE(z), MONE(x), 1) => { ONE(addThree(z, x, 0)) }
      case (ONE(z), ZERO(x), -1) => { ZERO(addThree(z, x, 0)) }
      case (ONE(z), ZERO(x), 0) => { ONE(addThree(z, x, 0)) }
      case (ONE(z), ZERO(x), 1) => { ZERO(addThree(z, x, 1)) }
      case (ONE(z), ONE(x), -1) => { ONE(addThree(z, x, 0)) }
      case (ONE(z), ONE(x), 0) => { ZERO(addThree(z, x, 1)) }
      case (ONE(z), ONE(x), 1) => { ONE(addThree(z, x, 1)) }
      case (ZERO(z), NIL, -1) => { MONE(z) }
      case (ZERO(z), NIL, 0) => { ZERO(z) }
      case (ZERO(z), NIL, 1) => { ONE(z) }
      case (ZERO(z), MONE(x), -1) => { ZERO(addThree(z, x, -(1))) }
      case (ZERO(z), MONE(x), 0) => { MONE(addThree(z, x, 0)) }
      case (ZERO(z), MONE(x), 1) => { ZERO(addThree(z, x, 0)) }
      case (ZERO(z), ZERO(x), -1) => { MONE(addThree(z, x, 0)) }
      case (ZERO(z), ZERO(x), 0) => { ZERO(addThree(z, x, 0)) }
      case (ZERO(z), ZERO(x), 1) => { ONE(addThree(z, x, 0)) }
      case (ZERO(z), ONE(x), -1) => { ZERO(addThree(z, x, 0)) }
      case (ZERO(z), ONE(x), 0) => { ONE(addThree(z, x, 0)) }
      case (ZERO(z), ONE(x), 1) => { ZERO(addThree(z, x, 1)) }
      case (MONE(z), NIL, -1) => { ZERO(addThree(z, NIL, -(1))) }
      case (MONE(z), NIL, 0) => { MONE(z) }
      case (MONE(z), NIL, 1) => { ZERO(z) }
      case (MONE(z), MONE(x), -1) => { MONE(addThree(z, x, -(1))) }
      case (MONE(z), MONE(x), 0) => { ZERO(addThree(z, x, -(1))) }
      case (MONE(z), MONE(x), 1) => { MONE(addThree(z, x, 0)) }
      case (MONE(z), ZERO(x), -1) => { ZERO(addThree(z, x, -(1))) }
      case (MONE(z), ZERO(x), 0) => { MONE(addThree(z, x, 0)) }
      case (MONE(z), ZERO(x), 1) => { ZERO(addThree(z, x, 0)) }
      case (MONE(z), ONE(x), -1) => { MONE(addThree(z, x, 0)) }
      case (MONE(z), ONE(x), 0) => { ZERO(addThree(z, x, 0)) }
      case (MONE(z), ONE(x), 1) => { ONE(addThree(z, x, 0)) }
    }
  }
  def crazy2add(((c1, c2))) = { addThree(c1, c2, 0) }
}