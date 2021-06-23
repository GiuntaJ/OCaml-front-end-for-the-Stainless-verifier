import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub54 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (x, y) =>
      {
        (x, y) match {
          case (NIL, y) => { y }
          case (x, NIL) => { x }
          case (ZERO(x2), ZERO(y2)) => { ZERO(crazy2add(x2, y2)) }
          case (ZERO(x2), ONE(y2)) => { ONE(crazy2add(x2, y2)) }
          case (ZERO(x2), MONE(y2)) => { MONE(crazy2add(x2, y2)) }
          case (ONE(x2), ZERO(y2)) => { ONE(crazy2add(x2, y2)) }
          case (MONE(x2), ZERO(y2)) => { MONE(crazy2add(x2, y2)) }
          case (ONE(x2), MONE(y2)) => { ZERO(crazy2add(x2, y2)) }
          case (MONE(x2), ONE(y2)) => { ZERO(crazy2add(x2, y2)) }
          case (ONE(x2), ONE(y2)) => {
            ZERO(crazy2add(ONE(NIL), crazy2add(x2, y2)))
          }
          case (MONE(x2), MONE(y2)) => {
            ZERO(crazy2add(MONE(NIL), crazy2add(x2, y2)))
          }
        }
    }
  }
  
}
