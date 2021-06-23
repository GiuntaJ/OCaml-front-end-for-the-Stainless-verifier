import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub75 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((left, right))) = {
    (left, right) match {
      case (_, NIL) => { left }
      case (NIL, _) => { right }
      case (ZERO(l), ONE(r)) => { ONE(crazy2add(l, r)) }
      case (MONE(l), ONE(r)) => { ZERO(crazy2add(l, r)) }
      case (ZERO(l), MONE(r)) => { MONE(crazy2add(l, r)) }
      case (ZERO(l), ZERO(r)) => { ZERO(crazy2add(l, r)) }
      case (ONE(l), ZERO(r)) => { ONE(crazy2add(l, r)) }
      case (ONE(l), MONE(r)) => { ZERO(crazy2add(l, r)) }
      case (MONE(l), ZERO(r)) => { MONE(crazy2add(l, r)) }
      case (ONE(l), ONE(r)) => { ZERO(crazy2add(crazy2add(l, r), ONE(NIL))) }
      case (MONE(l), MONE(r)) => { ZERO(crazy2add(crazy2add(l, r), MONE(NIL))) }
    }
  }
}