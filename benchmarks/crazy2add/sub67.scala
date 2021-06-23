import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub67 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  def crazy2add(((a, b))) = {
    (a, b) match {
      case (NIL, _) => { b }
      case (_, NIL) => { a }
      case (ZERO(aa), ZERO(bb)) => { ZERO(crazy2add(aa, bb)) }
      case (ONE(aa), MONE(bb)) => { ZERO(crazy2add(aa, bb)) }
      case (MONE(aa), ONE(bb)) => { ZERO(crazy2add(aa, bb)) }
      case (ZERO(aa), ONE(bb)) => { ONE(crazy2add(aa, bb)) }
      case (ONE(aa), ZERO(bb)) => { ONE(crazy2add(aa, bb)) }
      case (ZERO(aa), MONE(bb)) => { MONE(crazy2add(aa, bb)) }
      case (MONE(aa), ZERO(bb)) => { MONE(crazy2add(aa, bb)) }
      case (ONE(aa), ONE(bb)) => { ZERO(crazy2add(crazy2add(aa, ONE(NIL)), bb))
      }
      case (MONE(aa), MONE(bb)) => {
        ZERO(crazy2add(crazy2add(aa, MONE(NIL)), bb))
      }
    }
  }
}