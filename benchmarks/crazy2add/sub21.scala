import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub21 {
  /* 2006-11782 Song Young-chan, Hw1-7 crazy2add */
  
  sealed case class Error(param0: String) extends Exception {}
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add_in(((number1, number2))) = {
    (number1, number2) match {
      case (NIL, NIL) => { NIL }
      case (NIL, MONE(remain)) | (MONE(remain), NIL) => {
        MONE(crazy2add_in(NIL, remain))
      }
      case (NIL, ONE(remain)) | (ONE(remain), NIL) => {
        ONE(crazy2add_in(NIL, remain))
      }
      case (NIL, ZERO(remain)) | (ZERO(remain), NIL) => {
        ZERO(crazy2add_in(NIL, remain))
      }
      case (MONE(remain1), ONE(remain2)) | (ONE(remain1), MONE(remain2)) |
      (ZERO(remain1), ZERO(remain2)) => {
        ZERO(crazy2add_in(remain1, remain2))
      }
      case (MONE(remain1), ZERO(remain2)) | (ZERO(remain1), MONE(remain2)) => {
        MONE(crazy2add_in(remain1, remain2))
      }
      case (ONE(remain1), ZERO(remain2)) | (ZERO(remain1), ONE(remain2)) => {
        ONE(crazy2add_in(remain1, remain2))
      }
      case (MONE(remain1), MONE(remain2)) => {
        ZERO(crazy2add_in(crazy2add_in(MONE(NIL), remain1), remain2))
      }
      case (ONE(remain1), ONE(remain2)) => {
        ZERO(crazy2add_in(crazy2add_in(ONE(NIL), remain1), remain2))
      }
    }
  }
  	 
  def crazy2add(((number1: Crazy2, number2: Crazy2))) = {
    (number1, number2) match {
      case (NIL, _) | (_, NIL) => { assert(false, "Error with invalid arg") }
      case _ => { crazy2add_in(number1, number2) }
    }
  }
  
  val a: Crazy2 = ONE(ZERO(ONE(NIL)))
  val b: Crazy2 = MONE(ZERO(ZERO(NIL)))
}