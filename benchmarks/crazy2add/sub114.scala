import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub114 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    (c1, c2) match {
      case (NIL, NIL) => { NIL }
      case (c1, NIL) => { c1 }
      case (NIL, c2) => { c2 }
      case (ZERO(h1), ZERO(h2)) => { ZERO(crazy2add(h1, h2)) }
      case (ZERO(h1), ONE(h2)) => { ONE(crazy2add(h1, h2)) }
      case (ZERO(h1), MONE(h2)) => { MONE(crazy2add(h1, h2)) }
      case (ONE(h1), ZERO(h2)) => { ONE(crazy2add(h1, h2)) }
      case (ONE(h1), ONE(h2)) => { ZERO(crazy2add(crazy2add(h1, h2), ONE(NIL)))
      }
      case (ONE(h1), MONE(h2)) => { ZERO(crazy2add(h1, h2)) }
      case (MONE(h1), ZERO(h2)) => { MONE(crazy2add(h1, h2)) }
      case (MONE(h1), ONE(h2)) => { ZERO(crazy2add(h1, h2)) }
      case (MONE(h1), MONE(h2)) => {
        ZERO(crazy2add(crazy2add(h1, h2), MONE(NIL)))
      }
    }
  }
}