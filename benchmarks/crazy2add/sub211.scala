import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub211 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  val crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        val _2 = {
          def c2R: (Crazy2, Crazy2, Crazy2) => Crazy2 = {
            case (a, b, c) =>
              {
                a match {
                  case NIL => {
                    (b, c) match {
                      case (_, NIL) => { NIL }
                      case (NIL, ZERO(y)) => { NIL }
                      case (NIL, ONE(y)) => { ONE(NIL) }
                      case (NIL, MONE(y)) => { MONE(NIL) }
                      case (ZERO(x), ZERO(y)) => { ZERO(c2R(a, x, ZERO(NIL))) }
                      case (ZERO(x), ONE(y)) => { ONE(c2R(a, x, ZERO(NIL))) }
                      case (ZERO(x), MONE(y)) => { MONE(c2R(a, x, ZERO(NIL))) }
                      case (ONE(x), ZERO(y)) => { ONE(c2R(a, x, ZERO(NIL))) }
                      case (ONE(x), ONE(y)) => { ZERO(c2R(a, x, ONE(NIL))) }
                      case (ONE(x), MONE(y)) => { ZERO(c2R(a, x, ZERO(NIL))) }
                      case (MONE(x), ZERO(y)) => { MONE(c2R(a, x, ZERO(NIL))) }
                      case (MONE(x), ONE(y)) => { ZERO(c2R(a, x, ZERO(NIL))) }
                      case (MONE(x), MONE(y)) => { ZERO(c2R(a, x, MONE(NIL))) }
                    }
                  }
                  case ZERO(x) => {
                    (b, c) match {
                      case (_, NIL) => { NIL }
                      case (NIL, ZERO(z)) => { ZERO(c2R(x, NIL, ZERO(NIL))) }
                      case (NIL, ONE(z)) => { ONE(c2R(x, NIL, ZERO(NIL))) }
                      case (NIL, MONE(z)) => { MONE(c2R(x, NIL, ZERO(NIL))) }
                      case (ZERO(y), ZERO(z)) => { ZERO(c2R(x, y, ZERO(NIL))) }
                      case (ZERO(y), ONE(z)) => { ONE(c2R(x, y, ZERO(NIL))) }
                      case (ZERO(y), MONE(z)) => { MONE(c2R(x, y, ZERO(NIL))) }
                      case (ONE(y), ZERO(z)) => { ONE(c2R(x, y, ZERO(NIL))) }
                      case (ONE(y), ONE(z)) => { ZERO(c2R(x, y, ONE(NIL))) }
                      case (ONE(y), MONE(z)) => { ZERO(c2R(x, y, ZERO(NIL))) }
                      case (MONE(y), ZERO(z)) => { MONE(c2R(x, y, ZERO(NIL))) }
                      case (MONE(y), ONE(z)) => { ZERO(c2R(x, y, ZERO(NIL))) }
                      case (MONE(y), MONE(z)) => { ZERO(c2R(x, y, MONE(NIL))) }
                    }
                  }
                  case ONE(x) => {
                    (b, c) match {
                      case (_, NIL) => { NIL }
                      case (NIL, ZERO(z)) => { ONE(c2R(x, NIL, ZERO(NIL))) }
                      case (NIL, ONE(z)) => { ZERO(c2R(x, NIL, ONE(NIL))) }
                      case (NIL, MONE(z)) => { ZERO(c2R(x, NIL, ZERO(NIL))) }
                      case (ZERO(y), ZERO(z)) => { ONE(c2R(x, y, ZERO(NIL))) }
                      case (ZERO(y), ONE(z)) => { ZERO(c2R(x, y, ONE(NIL))) }
                      case (ZERO(y), MONE(z)) => { ZERO(c2R(x, y, ZERO(NIL))) }
                      case (ONE(y), ZERO(z)) => { ZERO(c2R(x, y, ONE(NIL))) }
                      case (ONE(y), ONE(z)) => { ONE(c2R(x, y, ONE(NIL))) }
                      case (ONE(y), MONE(z)) => { ONE(c2R(x, y, ZERO(NIL))) }
                      case (MONE(y), ZERO(z)) => { ZERO(c2R(x, y, ZERO(NIL))) }
                      case (MONE(y), ONE(z)) => { ONE(c2R(x, y, ZERO(NIL))) }
                      case (MONE(y), MONE(z)) => { MONE(c2R(x, y, ZERO(NIL))) }
                    }
                  }
                  case MONE(x) => {
                    (b, c) match {
                      case (_, NIL) => { NIL }
                      case (NIL, ZERO(z)) => { MONE(c2R(x, NIL, ZERO(NIL))) }
                      case (NIL, ONE(z)) => { ZERO(c2R(x, NIL, ZERO(NIL))) }
                      case (NIL, MONE(z)) => { ZERO(c2R(x, NIL, MONE(NIL))) }
                      case (ZERO(y), ZERO(z)) => { MONE(c2R(x, y, ZERO(NIL))) }
                      case (ZERO(y), ONE(z)) => { ZERO(c2R(x, y, ZERO(NIL))) }
                      case (ZERO(y), MONE(z)) => { ZERO(c2R(x, y, MONE(NIL))) }
                      case (ONE(y), ZERO(z)) => { ZERO(c2R(x, y, ZERO(NIL))) }
                      case (ONE(y), ONE(z)) => { ONE(c2R(x, y, ZERO(NIL))) }
                      case (ONE(y), MONE(z)) => { MONE(c2R(x, y, ZERO(NIL))) }
                      case (MONE(y), ZERO(z)) => { ZERO(c2R(x, y, MONE(NIL))) }
                      case (MONE(y), ONE(z)) => { MONE(c2R(x, y, ZERO(NIL))) }
                      case (MONE(y), MONE(z)) => { MONE(c2R(x, y, MONE(NIL))) }
                    }
                  }
                }
            }
          }
          c2R(a, b, ZERO(NIL))
        }
    }
  }
}