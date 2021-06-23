import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub225 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((cz1: Crazy2, cz2: Crazy2))): Crazy2 = {
    (cz1, cz2) match {
      case (NIL, _) => { cz2 }
      case (_, NIL) => { cz1 }
      case (ZERO(cz10), ZERO(cz20)) => { ZERO(crazy2add(cz10, cz20)) }
      case (ONE(cz10), MONE(cz20)) | (MONE(cz10), ONE(cz20)) => {
        ZERO(crazy2add(cz10, cz20))
      }
      case (ONE(cz10), ZERO(cz20)) | (ZERO(cz10), ONE(cz20)) => {
        ONE(crazy2add(cz10, cz20))
      }
      case (MONE(cz10), ZERO(cz20)) | (ZERO(cz10), MONE(cz20)) => {
        MONE(crazy2add(cz10, cz20))
      }
      case (ONE(cz10), ONE(cz20)) => {
        ZERO(crazy2add(crazy2add(cz10, ONE(NIL)), cz20))
      }
      case (MONE(cz10), MONE(cz20)) => {
        ZERO(crazy2add(crazy2add(cz10, MONE(NIL)), cz20))
      }
    }
  }
}