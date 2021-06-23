import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub102 {
  /*open ex02*/
   
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /*fuck is it right?*/
  def crazy2add(((left, right))) = {
    (left, right) match {
      case (NIL, _) => { right }
      case (_, NIL) => { left }
      case (ZERO(left_0), ZERO(right_0)) => { ZERO(crazy2add(left_0, right_0)) }
      case (ONE(left_0), ZERO(right_0)) => { ONE(crazy2add(left_0, right_0)) }
      case (ZERO(left_0), ONE(right_0)) => { ONE(crazy2add(left_0, right_0)) }
      case (ONE(left_0), ONE(right_0)) => {
        ZERO(ONE(crazy2add(left_0, right_0)))
      }
      case (MONE(left_0), ZERO(right_0)) => { MONE(crazy2add(left_0, right_0)) }
      case (ZERO(left_0), MONE(right_0)) => { MONE(crazy2add(left_0, right_0)) }
      case (MONE(left_0), MONE(right_0)) => {
        ZERO(MONE(crazy2add(left_0, right_0)))
      }
      case (ONE(left_0), MONE(right_0)) => { ZERO(crazy2add(left_0, right_0)) }
      case (MONE(left_0), ONE(right_0)) => { ZERO(crazy2add(left_0, right_0)) }
    }
  }
  
  /*
  let x = ZERO(ONE(MONE NIL))
  let y = ONE(MONE(ONE NIL))
  
  let _ = print_int (crazy2val(crazy2add ((x), (y))))
  let _ = print_int (crazy2val x)
  let _ = print_int (crazy2val y)
  */
}