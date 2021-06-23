import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub218 {
  /*컴퓨터공학부 2014-16775 김민지
  programming language hw 2-3*/
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  def crazy2overflow(((x: Crazy2, y: Crazy2, z: Crazy2))): Crazy2 = {
    (x, y, z) match {
      case (NIL, NIL, _) => { z }
      case (NIL, _, NIL) => { y }
      case (_, NIL, NIL) => { x }
      case (NIL, ONE(y1), ONE(z1)) => { ZERO(crazy2overflow(NIL, y1, ONE(NIL)))
      }
      case (NIL, ONE(y1), ZERO(z1)) => { ONE(crazy2overflow(NIL, y1, ZERO(NIL)))
      }
      case (NIL, ONE(y1), MONE(z1)) => {
        ZERO(crazy2overflow(NIL, y1, ZERO(NIL)))
      }
      case (NIL, ZERO(y1), ONE(z1)) => { ONE(crazy2overflow(NIL, y1, ZERO(NIL)))
      }
      case (NIL, ZERO(y1), ZERO(z1)) => {
        ZERO(crazy2overflow(NIL, y1, ZERO(NIL)))
      }
      case (NIL, ZERO(y1), MONE(z1)) => {
        MONE(crazy2overflow(NIL, y1, ZERO(NIL)))
      }
      case (NIL, MONE(y1), ONE(z1)) => {
        ZERO(crazy2overflow(NIL, y1, ZERO(NIL)))
      }
      case (NIL, MONE(y1), ZERO(z1)) => {
        MONE(crazy2overflow(NIL, y1, ZERO(NIL)))
      }
      case (NIL, MONE(y1), MONE(z1)) => {
        ZERO(crazy2overflow(NIL, y1, MONE(NIL)))
      }
      case (ONE(x1), NIL, ONE(z1)) => { ZERO(crazy2overflow(x1, NIL, ONE(NIL)))
      }
      case (ONE(x1), NIL, ZERO(z1)) => { ONE(crazy2overflow(x1, NIL, ZERO(NIL)))
      }
      case (ONE(x1), NIL, MONE(z1)) => {
        ZERO(crazy2overflow(x1, NIL, ZERO(NIL)))
      }
      case (ONE(x1), ONE(y1), NIL) => { ZERO(crazy2overflow(x1, y1, ONE(NIL))) }
      case (ONE(x1), ONE(y1), ONE(z1)) => {
        ONE(crazy2overflow(x1, y1, ONE(NIL)))
      }
      case (ONE(x1), ONE(y1), ZERO(z1)) => {
        ZERO(crazy2overflow(x1, y1, ONE(NIL)))
      }
      case (ONE(x1), ONE(y1), MONE(z1)) => {
        ONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ONE(x1), ZERO(y1), NIL) => { ONE(crazy2overflow(x1, y1, NIL)) }
      case (ONE(x1), ZERO(y1), ONE(z1)) => {
        ZERO(crazy2overflow(x1, y1, ONE(NIL)))
      }
      case (ONE(x1), ZERO(y1), ZERO(z1)) => {
        ONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ONE(x1), ZERO(y1), MONE(z1)) => {
        ZERO(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ONE(x1), MONE(y1), NIL) => { ZERO(crazy2overflow(x1, y1, NIL)) }
      case (ONE(x1), MONE(y1), ONE(z1)) => {
        ONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ONE(x1), MONE(y1), ZERO(z1)) => {
        ZERO(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ONE(x1), MONE(y1), MONE(z1)) => {
        MONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ZERO(x1), NIL, ONE(z1)) => { ONE(crazy2overflow(x1, NIL, ZERO(NIL)))
      }
      case (ZERO(x1), NIL, ZERO(z1)) => {
        ZERO(crazy2overflow(x1, NIL, ZERO(NIL)))
      }
      case (ZERO(x1), NIL, MONE(z1)) => {
        MONE(crazy2overflow(x1, NIL, ZERO(NIL)))
      }
      case (ZERO(x1), ONE(y1), NIL) => { ONE(crazy2overflow(x1, y1, NIL)) }
      case (ZERO(x1), ONE(y1), ONE(z1)) => {
        ZERO(crazy2overflow(x1, y1, ONE(NIL)))
      }
      case (ZERO(x1), ONE(y1), ZERO(z1)) => {
        ONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ZERO(x1), ONE(y1), MONE(z1)) => {
        ZERO(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ZERO(x1), ZERO(y1), NIL) => { ZERO(crazy2overflow(x1, y1, NIL)) }
      case (ZERO(x1), ZERO(y1), ONE(z1)) => {
        ONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ZERO(x1), ZERO(y1), ZERO(z1)) => {
        ZERO(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ZERO(x1), ZERO(y1), MONE(z1)) => {
        MONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ZERO(x1), MONE(y1), NIL) => { MONE(crazy2overflow(x1, y1, NIL)) }
      case (ZERO(x1), MONE(y1), ONE(z1)) => {
        ZERO(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ZERO(x1), MONE(y1), ZERO(z1)) => {
        MONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (ZERO(x1), MONE(y1), MONE(z1)) => {
        ZERO(crazy2overflow(x1, y1, MONE(NIL)))
      }
      case (MONE(x1), NIL, ONE(z1)) => {
        ZERO(crazy2overflow(x1, NIL, ZERO(NIL)))
      }
      case (MONE(x1), NIL, ZERO(z1)) => {
        MONE(crazy2overflow(x1, NIL, ZERO(NIL)))
      }
      case (MONE(x1), NIL, MONE(z1)) => {
        ZERO(crazy2overflow(x1, NIL, MONE(NIL)))
      }
      case (MONE(x1), ONE(y1), NIL) => { ZERO(crazy2overflow(x1, y1, NIL)) }
      case (MONE(x1), ONE(y1), ONE(z1)) => {
        ONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (MONE(x1), ONE(y1), ZERO(z1)) => {
        ZERO(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (MONE(x1), ONE(y1), MONE(z1)) => {
        MONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (MONE(x1), ZERO(y1), NIL) => { MONE(crazy2overflow(x1, y1, NIL)) }
      case (MONE(x1), ZERO(y1), ONE(z1)) => {
        ZERO(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (MONE(x1), ZERO(y1), ZERO(z1)) => {
        MONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (MONE(x1), ZERO(y1), MONE(z1)) => {
        ZERO(crazy2overflow(x1, y1, MONE(NIL)))
      }
      case (MONE(x1), MONE(y1), NIL) => {
        ZERO(crazy2overflow(x1, y1, MONE(NIL)))
      }
      case (MONE(x1), MONE(y1), ONE(z1)) => {
        MONE(crazy2overflow(x1, y1, ZERO(NIL)))
      }
      case (MONE(x1), MONE(y1), ZERO(z1)) => {
        ZERO(crazy2overflow(x1, y1, MONE(NIL)))
      }
      case (MONE(x1), MONE(y1), MONE(z1)) => {
        MONE(crazy2overflow(x1, y1, MONE(NIL)))
      }
    }
  }
  
  def crazy2add(((x: Crazy2, y: Crazy2))): Crazy2 = {
    (x, y) match {
      case (NIL, _) => { y }
      case (_, NIL) => { x }
      case (ZERO(x1), ZERO(y1)) => { ZERO(crazy2add(x1, y1)) }
      case (ZERO(x1), ONE(y1)) => { ONE(crazy2add(x1, y1)) }
      case (ZERO(x1), MONE(y1)) => { MONE(crazy2add(x1, y1)) }
      case (ONE(x1), ZERO(y1)) => { ONE(crazy2add(x1, y1)) }
      case (ONE(x1), ONE(y1)) => { ZERO(crazy2overflow(x1, y1, ONE(NIL))) }
      case (ONE(x1), MONE(y1)) => { ZERO(crazy2add(x1, y1)) }
      case (MONE(x1), ZERO(y1)) => { MONE(crazy2add(x1, y1)) }
      case (MONE(x1), ONE(y1)) => { ZERO(crazy2add(x1, y1)) }
      case (MONE(x1), MONE(y1)) => { ZERO(crazy2overflow(x1, y1, MONE(NIL))) }
    }
  }
}
