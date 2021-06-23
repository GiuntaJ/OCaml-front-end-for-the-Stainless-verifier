import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub26 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1, c2))) = {
    (c1, c2) match {
      case (NIL, v) => { v }
      case (v, NIL) => { v }
      case (ZERO(v1), ZERO(v2)) => { ZERO(crazy2add(v1, v2)) }
      case (ZERO(v1), ONE(v2)) => { ONE(crazy2add(v1, v2)) }
      case (ZERO(v1), MONE(v2)) => { MONE(crazy2add(v1, v2)) }
      case (ONE(v1), ZERO(v2)) => { ONE(crazy2add(v1, v2)) }
      case (ONE(v1), ONE(v2)) => { ZERO(crazy2add(ONE(NIL), crazy2add(v1, v2)))
      }
      case (ONE(v1), MONE(v2)) => { ZERO(crazy2add(v1, v2)) }
      case (MONE(v1), ZERO(v2)) => { MONE(crazy2add(v1, v2)) }
      case (MONE(v1), ONE(v2)) => { ZERO(crazy2add(v1, v2)) }
      case (MONE(v1), MONE(v2)) => {
        ZERO(crazy2add(MONE(NIL), crazy2add(v1, v2)))
      }
    }
  }
}
