import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub233 {
  /* HW2 Exercise 3 sum of k crazy numbers */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (num1_adding, num2_adding) =>
      {
        (num1_adding, num2_adding) match {
          case (NIL, _) => { num2_adding }
          case (_, NIL) => { num1_adding }
          case (ZERO(num1), ZERO(num2)) => { ZERO(crazy2add(num1, num2)) }
          case (ZERO(num1), ONE(num2)) => { ONE(crazy2add(num1, num2)) }
          case (ZERO(num1), MONE(num2)) => { MONE(crazy2add(num1, num2)) }
          case (ONE(num1), ZERO(num2)) => { ONE(crazy2add(num1, num2)) }
          case (ONE(num1), ONE(num2)) => {
            ZERO(crazy2add(crazy2add(num1, num2), ONE(NIL)))
          }
          case (ONE(num1), MONE(num2)) => { ZERO(crazy2add(num1, num2)) }
          case (MONE(num1), ZERO(num2)) => { MONE(crazy2add(num1, num2)) }
          case (MONE(num1), ONE(num2)) => { ZERO(crazy2add(num1, num2)) }
          case (MONE(num1), MONE(num2)) => {
            ZERO(crazy2add(crazy2add(num1, num2), MONE(NIL)))
          }
        }
    }
  }
}