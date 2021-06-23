import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub40 {
  /* hw2ex2.ml */
  
  
  
  
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  sealed case class CarryBound() extends Exception {}
  
  def carry_oneside(c: Crazy2, carry: Int63): Crazy2 = {
    (c, carry) match {
      case (NIL, 0) => { ZERO(NIL) }
      case (NIL, 1) => { ONE(NIL) }
      case (NIL, -1) => { MONE(NIL) }
      case (ZERO(sub_c), 0) => { ZERO(carry_oneside(sub_c, 0)) }
      case (ZERO(sub_c), 1) => { ONE(carry_oneside(sub_c, 0)) }
      case (ZERO(sub_c), -1) => { MONE(carry_oneside(sub_c, 0)) }
      case (ONE(sub_c), 0) => { ONE(carry_oneside(sub_c, 0)) }
      case (ONE(sub_c), 1) => { ZERO(carry_oneside(sub_c, 1)) }
      case (ONE(sub_c), -1) => { ZERO(carry_oneside(sub_c, 0)) }
      case (MONE(sub_c), 0) => { MONE(carry_oneside(sub_c, 0)) }
      case (MONE(sub_c), 1) => { ZERO(carry_oneside(sub_c, 0)) }
      case (MONE(sub_c), -1) => { ZERO(carry_oneside(sub_c, -(1))) }
      case (_, _) => { assert(false, "CarryBound") }
    }
  }
  
  
  
  def crazy2add_aux(c1: Crazy2, c2: Crazy2, carry: Int63): Crazy2 = {
    (c1, c2, carry) match {
      case (NIL, NIL, 0) => { ZERO(NIL) }
      case (NIL, NIL, 1) => { ONE(NIL) }
      case (NIL, NIL, -1) => { MONE(NIL) }
      case (NIL, _, _) => { carry_oneside(c2, carry) }
      case (_, NIL, _) => { carry_oneside(c1, carry) }
      case (ZERO(sub1), ZERO(sub2), 0) | (ZERO(sub1), ONE(sub2), -1) |
      (ZERO(sub1), MONE(sub2), 1) | (MONE(sub1), ZERO(sub2), 1) |
      (ONE(sub1), ZERO(sub2), -1) | (MONE(sub1), ONE(sub2), 0) => {
        ZERO(crazy2add_aux(sub1, sub2, 0))
      }
      case (ZERO(sub1), ZERO(sub2), 1) | (ZERO(sub1), ONE(sub2), 0) |
      (ONE(sub1), ZERO(sub2), 0) | (ONE(sub1), MONE(sub2), 1) |
      (ONE(sub1), ONE(sub2), -1) | (MONE(sub1), ONE(sub2), 1) => {
        ONE(crazy2add_aux(sub1, sub2, 0))
      }
      case (ONE(sub1), MONE(sub2), -1) | (ZERO(sub1), ZERO(sub2), -1) |
      (ONE(sub1), MONE(sub2), 0) | (ZERO(sub1), MONE(sub2), 0) |
      (MONE(sub1), ZERO(sub2), 0) | (MONE(sub1), ONE(sub2), -1) |
      (MONE(sub1), MONE(sub2), 1) => {
        MONE(crazy2add_aux(sub1, sub2, 0))
      }
      case (ZERO(sub1), ONE(sub2), 1) | (ONE(sub1), ZERO(sub2), 1) |
      (ONE(sub1), ONE(sub2), 0) => {
        ZERO(crazy2add_aux(sub1, sub2, 1))
      }
      case (ONE(sub1), ONE(sub2), 1) => { ONE(crazy2add_aux(sub1, sub2, 1)) }
      case (ZERO(sub1), MONE(sub2), -1) | (MONE(sub1), ZERO(sub2), -1) |
      (MONE(sub1), MONE(sub2), 0) => {
        ZERO(crazy2add_aux(sub1, sub2, -(1)))
      }
      case (MONE(sub1), MONE(sub2), -1) => {
        MONE(crazy2add_aux(sub1, sub2, -(1)))
      }
      case (_, _, _) => { assert(false, "CarryBound") }
    }
  }
  
  
  def crazy2add(((c1, c2))): Crazy2 = { crazy2add_aux(c1, c2, 0) }
}