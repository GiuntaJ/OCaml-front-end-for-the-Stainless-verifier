import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub174 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    (c1, c2) match {
      case (NIL, _) => { c2 }
      case (_, NIL) => { c1 }
      case (ZERO(c1s), ONE(c2s)) => { ONE(crazy2add(c1s, c2s)) }
      case (ZERO(c1s), MONE(c2s)) => { MONE(crazy2add(c1s, c2s)) }
      case (ONE(c1s), ZERO(c2s)) => { ONE(crazy2add(c1s, c2s)) }
      case (ZERO(c1s), ZERO(c2s)) => { ZERO(crazy2add(c1s, c2s)) }
      case (MONE(c1s), ZERO(c2s)) => { MONE(crazy2add(c1s, c2s)) }
      case (ONE(c1s), MONE(c2s)) => { ZERO(crazy2add(c1s, c2s)) }
      case (ONE(c1s), ONE(c2s)) => {
        ZERO(crazy2add(ONE(NIL), crazy2add(c1s, c2s)))
      }
      case (MONE(c1s), MONE(c2s)) => {
        ZERO(crazy2add(MONE(NIL), crazy2add(c1s, c2s)))
      }
      case (MONE(c1s), ONE(c2s)) => { ZERO(crazy2add(c1s, c2s)) }
    }
  }
}