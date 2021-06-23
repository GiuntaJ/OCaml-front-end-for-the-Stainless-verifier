import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub116 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def incr: Crazy2 => Crazy2 = (
    (a) =>
      {
        a match {
          case NIL => { ONE(NIL) }
          case ZERO(c) => { ONE(c) }
          case ONE(c) => { ZERO(incr(c)) }
          case MONE(c) => { ZERO(c) }
        }
    }
  )
  
  def decr: Crazy2 => Crazy2 = (
    (a) =>
      {
        a match {
          case NIL => { MONE(NIL) }
          case ZERO(c) => { MONE(c) }
          case ONE(c) => { ZERO(c) }
          case MONE(c) => { ZERO(decr(c)) }
        }
    }
  )
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        (a, b) match {
          case (NIL, d) => { b }
          case (c, NIL) => { a }
          case (ZERO(c), ZERO(d)) => { ZERO(crazy2add(c, d)) }
          case (ZERO(c), ONE(d)) => { ONE(crazy2add(c, d)) }
          case (ZERO(c), MONE(d)) => { MONE(crazy2add(c, d)) }
          case (ONE(c), ZERO(d)) => { ONE(crazy2add(c, d)) }
          case (ONE(c), ONE(d)) => { ZERO(crazy2add(c, incr(d))) }
          case (ONE(c), MONE(d)) => { ZERO(crazy2add(c, d)) }
          case (MONE(c), ZERO(d)) => { MONE(crazy2add(c, d)) }
          case (MONE(c), ONE(d)) => { ZERO(crazy2add(c, d)) }
          case (MONE(c), MONE(d)) => { ZERO(crazy2add(c, decr(d))) }
        }
    }
  }
}
