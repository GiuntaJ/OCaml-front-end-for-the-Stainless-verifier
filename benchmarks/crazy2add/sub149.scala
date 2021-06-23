import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub149 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        (a, b) match {
          case (NIL, NIL) => { NIL }
          case (_, NIL) => { a }
          case (NIL, _) => { b }
          case (ONE(ax), ONE(bx)) => {
            ZERO(crazy2add(crazy2add(ONE(NIL), ax), bx))
          }
          case (ONE(ax), ZERO(bx)) => { ONE(crazy2add(ax, bx)) }
          case (ONE(ax), MONE(bx)) => { ZERO(crazy2add(ax, bx)) }
          case (ZERO(ax), ONE(bx)) => { ONE(crazy2add(ax, bx)) }
          case (ZERO(ax), ZERO(bx)) => { ZERO(crazy2add(ax, bx)) }
          case (ZERO(ax), MONE(bx)) => { MONE(crazy2add(ax, bx)) }
          case (MONE(ax), ONE(bx)) => { ZERO(crazy2add(ax, bx)) }
          case (MONE(ax), ZERO(bx)) => { MONE(crazy2add(ax, bx)) }
          case (MONE(ax), MONE(bx)) => {
            ZERO(crazy2add(crazy2add(MONE(NIL), ax), bx))
          }
        }
    }
  }
}