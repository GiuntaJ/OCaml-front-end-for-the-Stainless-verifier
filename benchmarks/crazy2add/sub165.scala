import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub165 {
  /*
   CSE 2012-11226 Kwak Jin Han
   exercise 3
   */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /* crazy2val : crazy2 -> int */						
  
  /* crazy2add : crazy2 * crazy2 -> crazy2 */
  def crazy2add(((left, right))) = {
    (left, right) match {
      case (NIL, NIL) => { NIL }
      case (NIL, _) => { right }
      case (_, NIL) => { left }
      case (ZERO(lf), ZERO(rf)) => { ZERO(crazy2add(lf, rf)) }
      case (ZERO(lf), ONE(rf)) => { ONE(crazy2add(lf, rf)) }
      case (ZERO(lf), MONE(rf)) => { MONE(crazy2add(lf, rf)) }
      case (ONE(lf), ZERO(rf)) => { ONE(crazy2add(lf, rf)) }
      case (ONE(lf), ONE(rf)) => {
        crazy2add(ZERO(ONE(NIL)), ZERO(crazy2add(lf, rf)))
      }
      case (ONE(lf), MONE(rf)) => { ZERO(crazy2add(lf, rf)) }
      case (MONE(lf), ZERO(rf)) => { MONE(crazy2add(lf, rf)) }
      case (MONE(lf), ONE(rf)) => { ZERO(crazy2add(lf, rf)) }
      case (MONE(lf), MONE(rf)) => {
        crazy2add(ZERO(MONE(NIL)), ZERO(crazy2add(lf, rf)))
      }
    }
  }
  
  /*
  let x = ZERO (MONE (ONE NIL))
  let y = ONE (ONE (ONE NIL))
  
  let i = crazy2val (crazy2add (x, y))
  let _ = print_int i
  
  let j = crazy2val x + crazy2val y
  let _ = print_int j
  */
}