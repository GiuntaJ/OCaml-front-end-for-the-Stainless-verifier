import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub35 {
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1, c2))) = {
    (c1, c2) match {
      case (NIL, c2) => { c2 }
      case (c1, NIL) => { c1 }
      case (ZERO(tc1), ZERO(tc2)) => { ZERO(crazy2add(tc1, tc2)) }
      case (ZERO(tc1), ONE(tc2)) => { ONE(crazy2add(tc1, tc2)) }
      case (ZERO(tc1), MONE(tc2)) => { MONE(crazy2add(tc1, tc2)) }
      case (ONE(tc1), ZERO(tc2)) => { ONE(crazy2add(tc1, tc2)) }
      case (ONE(tc1), ONE(tc2)) => {
        ZERO(crazy2add(ONE(NIL), crazy2add(tc1, tc2)))
      }
      case (ONE(tc1), MONE(tc2)) => { ZERO(crazy2add(tc1, tc2)) }
      case (MONE(tc1), ZERO(tc2)) => { MONE(crazy2add(tc1, tc2)) }
      case (MONE(tc1), ONE(tc2)) => { ZERO(crazy2add(tc1, tc2)) }
      case (MONE(tc1), MONE(tc2)) => {
        ZERO(crazy2add(MONE(NIL), crazy2add(tc1, tc2)))
      }
    }
  }
}