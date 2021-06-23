import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub70 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        (c1, c2) match {
          case (NIL, _) => { c2 }
          case (_, NIL) => { c1 }
          case (ZERO(c1_0), ZERO(c2_0)) => { ZERO(crazy2add(c1_0, c2_0)) }
          case (ZERO(c1_0), ONE(c2_0)) => { ONE(crazy2add(c1_0, c2_0)) }
          case (ZERO(c1_0), MONE(c2_0)) => { MONE(crazy2add(c1_0, c2_0)) }
          case (ONE(c1_0), ZERO(c2_0)) => { ONE(crazy2add(c1_0, c2_0)) }
          case (ONE(c1_0), ONE(c2_0)) => {
            ZERO(crazy2add(crazy2add(ONE(NIL), c1_0), c2_0))
          }
          case (ONE(c1_0), MONE(c2_0)) => { ZERO(crazy2add(c1_0, c2_0)) }
          case (MONE(c1_0), ZERO(c2_0)) => { MONE(crazy2add(c1_0, c2_0)) }
          case (MONE(c1_0), ONE(c2_0)) => { ZERO(crazy2add(c1_0, c2_0)) }
          case (MONE(c1_0), MONE(c2_0)) => {
            ZERO(crazy2add(crazy2add(MONE(NIL), c1_0), c2_0))
          }
        }
    }
  }
}