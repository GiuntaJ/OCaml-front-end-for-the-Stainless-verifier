import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub171 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    (c1, c2) match {
      case (NIL, _) => { c2 }
      case (_, NIL) => { c1 }
      case (ZERO(_c1), ZERO(_c2)) => { ZERO(crazy2add(_c1, _c2)) }
      case (ZERO(_c1), ONE(_c2)) => { ONE(crazy2add(_c1, _c2)) }
      case (ZERO(_c1), MONE(_c2)) => { MONE(crazy2add(_c1, _c2)) }
      case (ONE(_c1), ZERO(_c2)) => { ONE(crazy2add(_c1, _c2)) }
      case (ONE(_c1), ONE(_c2)) => {
        ZERO(crazy2add(crazy2add(_c1, _c2), ONE(NIL)))
      }
      case (ONE(_c1), MONE(_c2)) => { ZERO(crazy2add(_c1, _c2)) }
      case (MONE(_c1), ZERO(_c2)) => { MONE(crazy2add(_c1, _c2)) }
      case (MONE(_c1), ONE(_c2)) => { ZERO(crazy2add(_c1, _c2)) }
      case (MONE(_c1), MONE(_c2)) => {
        ZERO(crazy2add(crazy2add(_c1, _c2), MONE(NIL)))
      }
    }
  }
}
