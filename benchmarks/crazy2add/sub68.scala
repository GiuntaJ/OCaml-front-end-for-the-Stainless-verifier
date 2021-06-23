import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub68 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        (a, b) match {
          case (ZERO(c), ZERO(d)) | (ONE(c), MONE(d)) | (MONE(c), ONE(d)) => {
            ZERO(crazy2add(c, d))
          }
          case (NIL, ZERO(d)) => { ZERO(crazy2add(NIL, d)) }
          case (ZERO(c), NIL) => { ZERO(crazy2add(c, NIL)) }
          case (ONE(c), ONE(d)) => {
            crazy2add(ZERO(crazy2add(c, d)), ZERO(ONE(NIL)))
          }
          case (MONE(c), MONE(d)) => {
            crazy2add(ZERO(crazy2add(c, d)), ZERO(MONE(NIL)))
          }
          case (ZERO(c), ONE(d)) | (ONE(c), ZERO(d)) => { ONE(crazy2add(c, d)) }
          case (NIL, ONE(d)) => { ONE(crazy2add(NIL, d)) }
          case (ONE(c), NIL) => { ONE(crazy2add(c, NIL)) }
          case (ZERO(c), MONE(d)) | (MONE(c), ZERO(d)) => {
            MONE(crazy2add(c, d))
          }
          case (MONE(c), NIL) => { MONE(crazy2add(c, NIL)) }
          case (NIL, MONE(d)) => { MONE(crazy2add(NIL, d)) }
          case (NIL, NIL) => { NIL }
        }
    }
  }
}