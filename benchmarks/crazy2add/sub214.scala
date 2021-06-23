import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub214 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    (c1, c2) match {
      case (_, NIL) => { c1 }
      case (NIL, _) => { c2 }
      case (ZERO(c1_next), ZERO(c2_next)) => { ZERO(crazy2add(c1_next, c2_next))
      }
      case (ZERO(c1_next), ONE(c2_next)) => { ONE(crazy2add(c1_next, c2_next)) }
      case (ZERO(c1_next), MONE(c2_next)) => { MONE(crazy2add(c1_next, c2_next))
      }
      case (ONE(c1_next), ZERO(c2_next)) => { ONE(crazy2add(c1_next, c2_next)) }
      case (ONE(c1_next), ONE(c2_next)) => {
        ZERO(crazy2add(crazy2add(c1_next, c2_next), ONE(NIL)))
      }
      case (ONE(c1_next), MONE(c2_next)) => { ZERO(crazy2add(c1_next, c2_next))
      }
      case (MONE(c1_next), ZERO(c2_next)) => { MONE(crazy2add(c1_next, c2_next))
      }
      case (MONE(c1_next), ONE(c2_next)) => { ZERO(crazy2add(c1_next, c2_next))
      }
      case (MONE(c1_next), MONE(c2_next)) => {
        ZERO(crazy2add(crazy2add(c1_next, c2_next), MONE(NIL)))
      }
    }
  }
}