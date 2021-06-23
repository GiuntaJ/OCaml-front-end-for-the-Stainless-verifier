import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub120 {
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    (c1, c2) match {
      case (NIL, _) => { c2 }
      case (_, NIL) => { c1 }
      case (ZERO(crazy_1), ZERO(crazy_2)) | (MONE(crazy_1), ONE(crazy_2)) |
      (ONE(crazy_1), MONE(crazy_2)) => {
        ZERO(crazy2add(crazy_1, crazy_2))
      }
      case (ONE(crazy_1), ZERO(crazy_2)) | (ZERO(crazy_1), ONE(crazy_2)) => {
        ONE(crazy2add(crazy_1, crazy_2))
      }
      case (MONE(crazy_1), ZERO(crazy_2)) | (ZERO(crazy_1), MONE(crazy_2)) => {
        MONE(crazy2add(crazy_1, crazy_2))
      }
      case (ONE(crazy_1), ONE(crazy_2)) => {
        ZERO(crazy2add(crazy2add(crazy_1, crazy_2), ONE(NIL)))
      }
      case (MONE(crazy_1), MONE(crazy_2)) => {
        ZERO(crazy2add(crazy2add(crazy_1, crazy_2), MONE(NIL)))
      }
    }
  }
}
