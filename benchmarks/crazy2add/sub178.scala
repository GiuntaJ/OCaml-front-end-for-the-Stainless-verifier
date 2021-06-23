import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub178 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((l: Crazy2, r: Crazy2))): Crazy2 = {
    (l, r) match {
      case (NIL, _) => { r }
      case (_, NIL) => { l }
      case (ZERO(l_), ZERO(r_)) => { ZERO(crazy2add(l_, r_)) }
      case (ZERO(l_), MONE(r_)) => { MONE(crazy2add(l_, r_)) }
      case (ZERO(l_), ONE(r_)) => { ONE(crazy2add(l_, r_)) }
      case (MONE(l_), ZERO(r_)) => { MONE(crazy2add(l_, r_)) }
      case (ONE(l_), ZERO(r_)) => { ONE(crazy2add(l_, r_)) }
      case (ONE(l_), MONE(r_)) => { ZERO(crazy2add(l_, r_)) }
      case (MONE(l_), ONE(r_)) => { ZERO(crazy2add(l_, r_)) }
      case (ONE(l_), ONE(r_)) => { ZERO(crazy2add(ONE(NIL), crazy2add(l_, r_)))
      }
      case (MONE(l_), MONE(r_)) => {
        ZERO(crazy2add(MONE(NIL), crazy2add(l_, r_)))
      }
    }
  }
}
