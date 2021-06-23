import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub90 {
  /* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
   * Programming Languages 2015 Fall
   * Homework 2, Exercise 2 */
  
  /* type crazy2 */
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /* plus one */
  def crazy2one(cz: Crazy2): Crazy2 = {
    cz match {
      case NIL => { ONE(NIL) }
      case ZERO(subcz) => { ONE(subcz) }
      case ONE(subcz) => { ZERO(crazy2one(subcz)) }
      case MONE(subcz) => { ZERO(subcz) }
    }
  }
  
  /* minus one */
  def crazy2mone(cz: Crazy2): Crazy2 = {
    cz match {
      case NIL => { MONE(NIL) }
      case ZERO(subcz) => { MONE(subcz) }
      case ONE(subcz) => { ZERO(subcz) }
      case MONE(subcz) => { ZERO(crazy2mone(subcz)) }
    }
  }
  
  /* crazy2add */
  def crazy2add(((a, b))) = {
    (a, b) match {
      case (NIL, _) => { b }
      case (_, NIL) => { a }
      case (ONE(sa), ONE(sb)) => { ZERO(crazy2add(crazy2one(sa), sb)) }
      case (ONE(sa), ZERO(sb)) => { ONE(crazy2add(sa, sb)) }
      case (ONE(sa), MONE(sb)) => { ZERO(crazy2add(sa, sb)) }
      case (ZERO(sa), ONE(sb)) => { ONE(crazy2add(sa, sb)) }
      case (ZERO(sa), ZERO(sb)) => { ZERO(crazy2add(sa, sb)) }
      case (ZERO(sa), MONE(sb)) => { MONE(crazy2add(sa, sb)) }
      case (MONE(sa), ONE(sb)) => { ZERO(crazy2add(sa, sb)) }
      case (MONE(sa), ZERO(sb)) => { MONE(crazy2add(sa, sb)) }
      case (MONE(sa), MONE(sb)) => { ZERO(crazy2add(crazy2mone(sa), sb)) }
    }
  }
}