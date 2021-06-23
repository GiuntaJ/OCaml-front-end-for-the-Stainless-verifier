import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub238 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((x, y))) = {
    (x, y) match {
      case (a, NIL) | (NIL, a) => { a }
      case (ZERO(a), ZERO(b)) | (ONE(a), MONE(b)) | (MONE(a), ONE(b)) => {
        ZERO(crazy2add(a, b))
      }
      case (ONE(a), ZERO(b)) | (ZERO(a), ONE(b)) => { ONE(crazy2add(a, b)) }
      case (MONE(a), ZERO(b)) | (ZERO(a), MONE(b)) => { MONE(crazy2add(a, b)) }
      case (ONE(a), ONE(b)) => { ZERO(crazy2add(crazy2add(a, b), ONE(NIL))) }
      case (MONE(a), MONE(b)) => { ZERO(crazy2add(crazy2add(a, b), MONE(NIL))) }
    }
  }
}