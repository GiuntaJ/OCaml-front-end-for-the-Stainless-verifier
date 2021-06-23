import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub200 {
  /* 컴퓨터공학부 2013-11425 이창영 hw2_3 */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def up(c: Crazy2): Crazy2 = {
    c match {
      case NIL => { NIL }
      case MONE(a) => { ZERO(a) }
      case ZERO(a) => { ONE(a) }
      case ONE(a) => { ZERO(up(a)) }
    }
  }
  
  def down(c: Crazy2): Crazy2 = {
    c match {
      case NIL => { NIL }
      case ONE(a) => { ZERO(a) }
      case ZERO(a) => { MONE(a) }
      case MONE(a) => { ZERO(down(a)) }
    }
  }
  
  def crazy2add(((a: Crazy2, b: Crazy2))): Crazy2 = {
    (a, b) match {
      case (NIL, NIL) => { ZERO(NIL) }
      case (_, NIL) => { a }
      case (NIL, _) => { b }
      case (ZERO(c), ZERO(d)) => { ZERO(crazy2add(c, d)) }
      case (ZERO(c), ONE(d)) => { ONE(crazy2add(c, d)) }
      case (ONE(c), ZERO(d)) => { ONE(crazy2add(c, d)) }
      case (ZERO(c), MONE(d)) => { MONE(crazy2add(c, d)) }
      case (MONE(c), ZERO(d)) => { MONE(crazy2add(c, d)) }
      case (ONE(c), ONE(d)) => { ZERO(up(crazy2add(c, d))) }
      case (MONE(c), MONE(d)) => { ZERO(down(crazy2add(c, d))) }
      case (MONE(c), ONE(d)) => { ZERO(crazy2add(c, d)) }
      case (ONE(c), MONE(d)) => { ZERO(crazy2add(c, d)) }
    }
  }
}