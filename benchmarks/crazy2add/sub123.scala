import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub123 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        (c1, c2) match {
          case (_, NIL) => { c1 }
          case (NIL, _) => { c2 }
          case (ZERO(x1), ZERO(x2)) => { ZERO(crazy2add(x1, x2)) }
          case (ZERO(x1), ONE(x2)) | (ONE(x1), ZERO(x2)) => {
            ONE(crazy2add(x1, x2))
          }
          case (ZERO(x1), MONE(x2)) | (MONE(x1), ZERO(x2)) => {
            MONE(crazy2add(x1, x2))
          }
          case (ONE(x1), MONE(x2)) | (MONE(x1), ONE(x2)) => {
            ZERO(crazy2add(x1, x2))
          }
          case (ONE(x1), ONE(x2)) => {
            ZERO(crazy2add(ONE(NIL), crazy2add(x1, x2)))
          }
          case (MONE(x1), MONE(x2)) => {
            ZERO(crazy2add(MONE(NIL), crazy2add(x1, x2)))
          }
        }
    }
  }
}