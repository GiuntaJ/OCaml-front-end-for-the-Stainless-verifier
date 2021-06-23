import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub219 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed case class Invalid_state(param0: String) extends Exception {}
  
  /* ASSERT: c is not nil */
  def splitHeadAndTail(c: Crazy2): (Int63, Crazy2) = {
    c match {
      case NIL => { assert(false, "Invalid_state with Invalid NIL") }
      case ZERO(c_0) => { (0, c_0) }
      case ONE(c_0) => { (1, c_0) }
      case MONE(c_0) => { (-(1), c_0) }
    }
  }
  
  def crazy2add_0(c1: Crazy2, c2: Crazy2, carry: Int63): Crazy2 = {
    (c1, c2, carry) match {
      case (NIL, _, 0) => { c2 }
      case (NIL, _, 1) => { crazy2add_0(c2, ONE(NIL), 0) }
      case (NIL, _, -1) => { crazy2add_0(c2, MONE(NIL), 0) }
      case (_, NIL, 0) => { c1 }
      case (_, NIL, 1) => { crazy2add_0(c1, ONE(NIL), 0) }
      case (_, NIL, -1) => { crazy2add_0(c1, MONE(NIL), 0) }
      case _ => {
        val _2 = {
          val ((h1, t1)) = splitHeadAndTail(c1)
          val _3 = {
            val ((h2, t2)) = splitHeadAndTail(c2)
            carry + h1 + h2 match {
              case 3 => { ONE(crazy2add_0(t1, t2, 1)) }
              case 2 => { ZERO(crazy2add_0(t1, t2, 1)) }
              case 1 => { ONE(crazy2add_0(t1, t2, 0)) }
              case 0 => { ZERO(crazy2add_0(t1, t2, 0)) }
              case -1 => { MONE(crazy2add_0(t1, t2, 0)) }
              case -2 => { ZERO(crazy2add_0(t1, t2, -(1))) }
              case -3 => { MONE(crazy2add_0(t1, t2, -(1))) }
              case _ => {
                assert(false, "Invalid_state with Invalid calculation result")
              }
            }
          }
        }
      }
    }
  }
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = { crazy2add_0(c1, c2, 0) }
}