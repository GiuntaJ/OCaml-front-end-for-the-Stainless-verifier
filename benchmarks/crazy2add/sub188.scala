import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub188 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (x, y) =>
      {
        (x, y) match {
          case (x, NIL) => { x }
          case (NIL, y) => { y }
          case (ZERO(a), ZERO(b)) => { ZERO(crazy2add(a, b)) }
          case (ZERO(a), ONE(b)) => { ONE(crazy2add(a, b)) }
          case (ZERO(a), MONE(b)) => { MONE(crazy2add(a, b)) }
          case (ONE(a), ZERO(b)) => { ONE(crazy2add(a, b)) }
          case (ONE(a), ONE(b)) => { ZERO(crazy2add(ONE(NIL), crazy2add(a, b)))
          }
          case (ONE(a), MONE(b)) => { ZERO(crazy2add(a, b)) }
          case (MONE(a), ZERO(b)) => { MONE(crazy2add(a, b)) }
          case (MONE(a), ONE(b)) => { ZERO(crazy2add(a, b)) }
          case (MONE(a), MONE(b)) => {
            ZERO(crazy2add(MONE(NIL), crazy2add(a, b)))
          }
        }
    }
  }
}