import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub119 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        (a, b) match {
          case (NIL, _) => { b }
          case (_, NIL) => { a }
          case (ZERO(a2), ZERO(b2)) => { ZERO(crazy2add(a2, b2)) }
          case (ONE(a2), ONE(b2)) => {
            ZERO(crazy2add(a2, crazy2add(b2, ONE(NIL))))
          }
          case (MONE(a2), MONE(b2)) => {
            ZERO(crazy2add(a2, crazy2add(b2, MONE(NIL))))
          }
          case (ZERO(a2), ONE(b2)) | (ONE(a2), ZERO(b2)) => {
            ONE(crazy2add(a2, b2))
          }
          case (ZERO(a2), MONE(b2)) | (MONE(a2), ZERO(b2)) => {
            MONE(crazy2add(a2, b2))
          }
          case (ONE(a2), MONE(b2)) | (MONE(a2), ONE(b2)) => {
            ZERO(crazy2add(a2, b2))
          }
        }
    }
  }
}