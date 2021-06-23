import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub153 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    val _2 = {
      def op1(((a, b))) = {
        val _5 = {
          def op2(((a, b, c))) = {
            (a, b, c) match {
              case (NIL, NIL, 1) => { ONE(NIL) }
              case (NIL, NIL, -1) => { MONE(NIL) }
              case (NIL, ZERO(b_0), 1) => { ONE(b_0) }
              case (NIL, ONE(b_0), 1) => { ZERO(op1(b_0, ONE(NIL))) }
              case (NIL, MONE(b_0), 1) => { ZERO(b_0) }
              case (NIL, ZERO(b_0), -1) => { MONE(b_0) }
              case (NIL, ONE(b_0), -1) => { ZERO(b_0) }
              case (NIL, MONE(b_0), -1) => { ZERO(op1(b_0, MONE(NIL))) }
              case (ZERO(a_0), NIL, 1) => { ONE(a_0) }
              case (ZERO(a_0), ZERO(b_0), 1) => { ONE(op1(a_0, b_0)) }
              case (ZERO(a_0), ONE(b_0), 1) => { ZERO(op2(a_0, b_0, 1)) }
              case (ZERO(a_0), MONE(b_0), 1) => { ZERO(op1(a_0, b_0)) }
              case (ZERO(a_0), NIL, -1) => { MONE(a_0) }
              case (ZERO(a_0), ZERO(b_0), -1) => { MONE(op1(a_0, b_0)) }
              case (ZERO(a_0), ONE(b_0), -1) => { ZERO(op1(a_0, b_0)) }
              case (ZERO(a_0), MONE(b_0), -1) => { ZERO(op2(a_0, b_0, -(1))) }
              case (ONE(a_0), NIL, 1) => { ZERO(op1(a_0, ONE(NIL))) }
              case (ONE(a_0), ZERO(b_0), 1) => { ZERO(op2(a_0, b_0, 1)) }
              case (ONE(a_0), ONE(b_0), 1) => { ONE(op2(a_0, b_0, 1)) }
              case (ONE(a_0), MONE(b_0), 1) => { ONE(op1(a_0, b_0)) }
              case (ONE(a_0), NIL, -1) => { ZERO(a_0) }
              case (ONE(a_0), ZERO(b_0), -1) => { ZERO(op1(a_0, b_0)) }
              case (ONE(a_0), ONE(b_0), -1) => { ONE(op1(a_0, b_0)) }
              case (ONE(a_0), MONE(b_0), -1) => { MONE(op1(a_0, b_0)) }
              case (MONE(a_0), NIL, 1) => { ZERO(a_0) }
              case (MONE(a_0), ZERO(b_0), 1) => { ZERO(op1(a_0, b_0)) }
              case (MONE(a_0), ONE(b_0), 1) => { ONE(op1(a_0, b_0)) }
              case (MONE(a_0), MONE(b_0), 1) => { MONE(op1(a_0, b_0)) }
              case (MONE(a_0), NIL, -1) => { ZERO(op1(a_0, MONE(NIL))) }
              case (MONE(a_0), ZERO(b_0), -1) => { ZERO(op2(a_0, b_0, -(1))) }
              case (MONE(a_0), ONE(b_0), -1) => { MONE(op1(a_0, b_0)) }
              case (MONE(a_0), MONE(b_0), -1) => { MONE(op2(a_0, b_0, -(1))) }
              case (_, _, _) => { op1(a, b) }
            }
          }
          (a, b) match {
            case (NIL, NIL) => { NIL }
            case (NIL, _) => { b }
            case (_, NIL) => { a }
            case (ZERO(a_0), ZERO(b_0)) => { ZERO(op1(a_0, b_0)) }
            case (ZERO(a_0), ONE(b_0)) => { ONE(op1(a_0, b_0)) }
            case (ZERO(a_0), MONE(b_0)) => { MONE(op1(a_0, b_0)) }
            case (ONE(a_0), ZERO(b_0)) => { ONE(op1(a_0, b_0)) }
            case (MONE(a_0), ZERO(b_0)) => { MONE(op1(a_0, b_0)) }
            case (ONE(a_0), ONE(b_0)) => { ZERO(op2(a_0, b_0, 1)) }
            case (ONE(a_0), MONE(b_0)) => { ZERO(op1(a_0, b_0)) }
            case (MONE(a_0), ONE(b_0)) => { ZERO(op1(a_0, b_0)) }
            case (MONE(a_0), MONE(b_0)) => { ZERO(op2(a_0, b_0, -(1))) }
          }
        }
      }
      op1(c1, c2)
    }
  }
}