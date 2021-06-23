import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub223 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((x: Crazy2, y: Crazy2))): Crazy2 = {
    (x, y) match {
      case (NIL, a) | (a, NIL) => { a }
      case (MONE(a), MONE(b)) => { ZERO(crazy2add(crazy2add(a, b), MONE(NIL))) }
      case (ONE(a), ONE(b)) => { ZERO(crazy2add(crazy2add(a, b), ONE(NIL))) }
      case (MONE(a), ONE(b)) | (ZERO(a), ZERO(b)) | (ONE(a), MONE(b)) => {
        ZERO(crazy2add(a, b))
      }
      case (MONE(a), ZERO(b)) | (ZERO(a), MONE(b)) => { MONE(crazy2add(a, b)) }
      case (ONE(a), ZERO(b)) | (ZERO(a), ONE(b)) => { ONE(crazy2add(a, b)) }
    }
  }
}