import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub199 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        (a, b) match {
          case (NIL, NIL) => { NIL }
          case (ONE(c), ONE(d)) => { ONE(ZERO(crazy2add(c, d))) }
          case (MONE(c), MONE(d)) => { MONE(ZERO(crazy2add(c, d))) }
          case (ZERO(c), ONE(d)) | (ONE(c), ZERO(d)) => { ONE(crazy2add(c, d)) }
          case (ZERO(c), MONE(d)) | (MONE(c), ZERO(d)) => {
            MONE(crazy2add(c, d))
          }
          case (NIL, ONE(c)) | (ONE(c), NIL) => { ONE(crazy2add(c, NIL)) }
          case (NIL, MONE(c)) | (MONE(c), NIL) => { MONE(crazy2add(c, NIL)) }
          case (ZERO(c), ZERO(d)) | (ONE(c), MONE(d)) | (MONE(c), ONE(d)) => {
            ZERO(crazy2add(c, d))
          }
          case (ZERO(c), NIL) | (NIL, ZERO(c)) => { ZERO(crazy2add(c, NIL)) }
        }
    }
  }
}