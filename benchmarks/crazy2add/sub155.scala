import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub155 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  def crazy2add(((a, b))) = {
    (a, b) match {
      case (NIL, NIL) => { NIL }
      case (NIL, c) => { c }
      case (c, NIL) => { c }
      case (ONE(c1), ONE(c2)) => { ZERO(crazy2add(crazy2add(c1, c2), ONE(NIL)))
      }
      case (ONE(c1), ZERO(c2)) => { ONE(crazy2add(c1, c2)) }
      case (ONE(c1), MONE(c2)) => { ZERO(crazy2add(c1, c2)) }
      case (ZERO(c1), ONE(c2)) => { ONE(crazy2add(c1, c2)) }
      case (ZERO(c1), ZERO(c2)) => { ZERO(crazy2add(c1, c2)) }
      case (ZERO(c1), MONE(c2)) => { MONE(crazy2add(c1, c2)) }
      case (MONE(c1), ONE(c2)) => { ZERO(crazy2add(c1, c2)) }
      case (MONE(c1), ZERO(c2)) => { MONE(crazy2add(c1, c2)) }
      case (MONE(c1), MONE(c2)) => {
        ZERO(crazy2add(crazy2add(c1, c2), MONE(NIL)))
      }
    }
  }
  
}
