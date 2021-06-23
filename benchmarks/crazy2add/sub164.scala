import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub164 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((a, b))) = {
    (a, b) match {
      case (ONE(c), ONE(d)) => { ZERO(crazy2add(crazy2add(ONE(NIL), c), d)) }
      case (MONE(c), MONE(d)) => { ZERO(crazy2add(crazy2add(MONE(NIL), c), d)) }
      case (ZERO(c), ZERO(d)) | (ONE(c), MONE(d)) => { ZERO(crazy2add(c, d)) }
      case (ZERO(c), ONE(d)) => { ONE(crazy2add(c, d)) }
      case (ZERO(c), MONE(d)) => { MONE(crazy2add(c, d)) }
      case (NIL, b) => { b }
      case (_, _) => { crazy2add(b, a) }
    }
  }
}