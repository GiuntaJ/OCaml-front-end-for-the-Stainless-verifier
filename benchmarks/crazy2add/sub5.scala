import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub5 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  sealed case class Error(param0: String) extends Exception {}
  def crazy2add(((a, b))) = {
    val _2 = {
      def crazy2adder(e) = {
        e match {
          case (NIL, NIL, ZERO(NIL)) => { NIL }
          case (NIL, NIL, ONE(NIL)) => { ONE(NIL) }
          case (NIL, NIL, MONE(NIL)) => { MONE(NIL) }
          case (ZERO(x), NIL, ZERO(NIL)) => {
            ZERO(crazy2adder(x, NIL, ZERO(NIL)))
          }
          case (ZERO(x), NIL, ONE(NIL)) => { ONE(crazy2adder(x, NIL, ZERO(NIL)))
          }
          case (ZERO(x), NIL, MONE(NIL)) => {
            MONE(crazy2adder(x, NIL, ZERO(NIL)))
          }
          case (NIL, ZERO(y), ZERO(NIL)) => {
            ZERO(crazy2adder(NIL, y, ZERO(NIL)))
          }
          case (NIL, ZERO(y), ONE(NIL)) => { ONE(crazy2adder(NIL, y, ZERO(NIL)))
          }
          case (NIL, ZERO(y), MONE(NIL)) => {
            MONE(crazy2adder(NIL, y, ZERO(NIL)))
          }
          case (ONE(x), NIL, ZERO(NIL)) => { ONE(crazy2adder(x, NIL, ZERO(NIL)))
          }
          case (ONE(x), NIL, ONE(NIL)) => { ZERO(crazy2adder(x, NIL, ONE(NIL)))
          }
          case (ONE(x), NIL, MONE(NIL)) => {
            ZERO(crazy2adder(x, NIL, ZERO(NIL)))
          }
          case (NIL, ONE(y), ZERO(NIL)) => { ONE(crazy2adder(NIL, y, ZERO(NIL)))
          }
          case (NIL, ONE(y), ONE(NIL)) => { ZERO(crazy2adder(NIL, y, ONE(NIL)))
          }
          case (NIL, ONE(y), MONE(NIL)) => {
            ZERO(crazy2adder(NIL, y, ZERO(NIL)))
          }
          case (MONE(x), NIL, ZERO(NIL)) => {
            MONE(crazy2adder(x, NIL, ZERO(NIL)))
          }
          case (MONE(x), NIL, ONE(NIL)) => {
            ZERO(crazy2adder(x, NIL, ZERO(NIL)))
          }
          case (MONE(x), NIL, MONE(NIL)) => {
            ZERO(crazy2adder(x, NIL, MONE(NIL)))
          }
          case (NIL, MONE(y), ZERO(NIL)) => {
            MONE(crazy2adder(NIL, y, ZERO(NIL)))
          }
          case (NIL, MONE(y), ONE(NIL)) => {
            ZERO(crazy2adder(NIL, y, ZERO(NIL)))
          }
          case (NIL, MONE(y), MONE(NIL)) => {
            ZERO(crazy2adder(NIL, y, MONE(NIL)))
          }
          case (ZERO(x), ZERO(y), ZERO(NIL)) => {
            ZERO(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ZERO(x), ZERO(y), ONE(NIL)) => {
            ONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ZERO(x), ZERO(y), MONE(NIL)) => {
            MONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ZERO(x), ONE(y), ZERO(NIL)) => {
            ONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ZERO(x), ONE(y), ONE(NIL)) => {
            ZERO(crazy2adder(x, y, ONE(NIL)))
          }
          case (ZERO(x), ONE(y), MONE(NIL)) => {
            ZERO(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ONE(x), ZERO(y), ZERO(NIL)) => {
            ONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ONE(x), ZERO(y), ONE(NIL)) => {
            ZERO(crazy2adder(x, y, ONE(NIL)))
          }
          case (ONE(x), ZERO(y), MONE(NIL)) => {
            ZERO(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ZERO(x), MONE(y), ZERO(NIL)) => {
            MONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ZERO(x), MONE(y), ONE(NIL)) => {
            ZERO(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ZERO(x), MONE(y), MONE(NIL)) => {
            ZERO(crazy2adder(x, y, MONE(NIL)))
          }
          case (MONE(x), ZERO(y), ZERO(NIL)) => {
            MONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (MONE(x), ZERO(y), ONE(NIL)) => {
            ZERO(crazy2adder(x, y, ZERO(NIL)))
          }
          case (MONE(x), ZERO(y), MONE(NIL)) => {
            ZERO(crazy2adder(x, y, MONE(NIL)))
          }
          case (ONE(x), ONE(y), ZERO(NIL)) => {
            ZERO(crazy2adder(x, y, ONE(NIL)))
          }
          case (ONE(x), ONE(y), ONE(NIL)) => { ONE(crazy2adder(x, y, ONE(NIL)))
          }
          case (ONE(x), ONE(y), MONE(NIL)) => {
            ONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ONE(x), MONE(y), ZERO(NIL)) => {
            ZERO(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ONE(x), MONE(y), ONE(NIL)) => {
            ONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (ONE(x), MONE(y), MONE(NIL)) => {
            MONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (MONE(x), ONE(y), ZERO(NIL)) => {
            ZERO(crazy2adder(x, y, ZERO(NIL)))
          }
          case (MONE(x), ONE(y), ONE(NIL)) => {
            ONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (MONE(x), ONE(y), MONE(NIL)) => {
            MONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (MONE(x), MONE(y), ZERO(NIL)) => {
            ZERO(crazy2adder(x, y, MONE(NIL)))
          }
          case (MONE(x), MONE(y), ONE(NIL)) => {
            MONE(crazy2adder(x, y, ZERO(NIL)))
          }
          case (MONE(x), MONE(y), MONE(NIL)) => {
            MONE(crazy2adder(x, y, MONE(NIL)))
          }
          case _ => { assert(false, "Error with invalid arg") }
        }
      }
      crazy2adder(a, b, ZERO(NIL))
    }
  }
}