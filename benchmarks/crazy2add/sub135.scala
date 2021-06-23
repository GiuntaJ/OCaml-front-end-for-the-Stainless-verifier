import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub135 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (x, y) =>
      {
        val _2 = {
          def plus: Crazy2 => Crazy2 = (
            (x) =>
              {
                x match {
                  case NIL => { ONE(NIL) }
                  case ZERO(x1) => { ONE(x1) }
                  case MONE(x1) => { ZERO(x1) }
                  case ONE(x1) => { ZERO(plus(x1)) }
                }
            }
          )
          val _3 = {
            def minus: Crazy2 => Crazy2 = (
              (x) =>
                {
                  x match {
                    case NIL => { MONE(NIL) }
                    case ZERO(x1) => { MONE(x1) }
                    case MONE(x1) => { ZERO(minus(x1)) }
                    case ONE(x1) => { ZERO(x1) }
                  }
              }
            )
            (x, y) match {
              case (NIL, y) => { y }
              case (x, NIL) => { x }
              case (ZERO(x1), ZERO(y1)) => { ZERO(crazy2add(x1, y1)) }
              case (ZERO(x1), ONE(y1)) => { ONE(crazy2add(x1, y1)) }
              case (ZERO(x1), MONE(y1)) => { MONE(crazy2add(x1, y1)) }
              case (ONE(x1), ZERO(y1)) => { ONE(crazy2add(x1, y1)) }
              case (ONE(x1), MONE(y1)) => { ZERO(crazy2add(x1, y1)) }
              case (ONE(x1), ONE(y1)) => { ZERO(crazy2add(plus(x1), y1)) }
              case (MONE(x1), MONE(y1)) => { ZERO(crazy2add(minus(x1), y1)) }
              case (MONE(x1), ZERO(y1)) => { MONE(crazy2add(x1, y1)) }
              case (MONE(x1), ONE(y1)) => { ZERO(crazy2add(x1, y1)) }
            }
          }
        }
    }
  }
}