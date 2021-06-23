import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub23 {
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        (c1, c2) match {
          case (NIL, a) => { a }
          case (a, NIL) => { a }
          case (ZERO(k1), ZERO(k2)) => { ZERO(crazy2add(k1, k2)) }
          case (ZERO(k1), ONE(k2)) => { ONE(crazy2add(k1, k2)) }
          case (ZERO(k1), MONE(k2)) => { MONE(crazy2add(k1, k2)) }
          case (ONE(k1), ZERO(k2)) => { ONE(crazy2add(k1, k2)) }
          case (ONE(k1), ONE(k2)) => {
            ZERO(crazy2add(crazy2add(ONE(NIL), k1), k2))
          }
          case (ONE(k1), MONE(k2)) => { ZERO(crazy2add(k1, k2)) }
          case (MONE(k1), ZERO(k2)) => { MONE(crazy2add(k1, k2)) }
          case (MONE(k1), ONE(k2)) => { ZERO(crazy2add(k1, k2)) }
          case (MONE(k1), MONE(k2)) => {
            ZERO(crazy2add(crazy2add(MONE(NIL), k1), k2))
          }
        }
    }
  }
}
