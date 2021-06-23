import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub152 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        (c1, c2) match {
          case (NIL, x) | (x, NIL) => { x }
          case (ZERO(x), MONE(y)) | (MONE(x), ZERO(y)) => {
            MONE(crazy2add(x, y))
          }
          case (MONE(x), ONE(y)) | (ZERO(x), ZERO(y)) | (ONE(x), MONE(y)) => {
            ZERO(crazy2add(x, y))
          }
          case (ZERO(x), ONE(y)) | (ONE(x), ZERO(y)) => { ONE(crazy2add(x, y)) }
          case (MONE(x), MONE(y)) => {
            ZERO(crazy2add(MONE(NIL), crazy2add(x, y)))
          }
          case (ONE(x), ONE(y)) => { ZERO(crazy2add(ONE(NIL), crazy2add(x, y)))
          }
        }
    }
  }
}