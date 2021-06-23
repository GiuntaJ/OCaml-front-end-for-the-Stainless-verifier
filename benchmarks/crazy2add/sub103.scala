import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub103 {
  /* Homework 2 - Exercise 3
   * 2011-10492 Jaeyeong Yang */
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (x, y) =>
      {
        val _2 = {
          val trimmed_zero: Crazy2 => Crazy2 = (
            (c) =>
              {
                c match {
                  case NIL => { NIL }
                  case _ => { ZERO(c) }
                }
            }
          )
          (x, y) match {
            case (NIL, NIL) => { NIL }
            case (NIL, y) => { y }
            case (x, NIL) => { x }
            case (ZERO(xl), ZERO(yl)) => { trimmed_zero(crazy2add(xl, yl)) }
            case (ONE(xl), MONE(yl)) => { trimmed_zero(crazy2add(xl, yl)) }
            case (MONE(xl), ONE(yl)) => { trimmed_zero(crazy2add(xl, yl)) }
            case (ZERO(xl), ONE(yl)) => { ONE(crazy2add(xl, yl)) }
            case (ONE(xl), ZERO(yl)) => { ONE(crazy2add(xl, yl)) }
            case (ZERO(xl), MONE(yl)) => { MONE(crazy2add(xl, yl)) }
            case (MONE(xl), ZERO(yl)) => { MONE(crazy2add(xl, yl)) }
            case (MONE(xl), MONE(yl)) => {
              trimmed_zero(crazy2add(crazy2add(xl, yl), MONE(NIL)))
            }
            case (ONE(xl), ONE(yl)) => {
              trimmed_zero(crazy2add(crazy2add(xl, yl), ONE(NIL)))
            }
          }
        }
    }
  }
}