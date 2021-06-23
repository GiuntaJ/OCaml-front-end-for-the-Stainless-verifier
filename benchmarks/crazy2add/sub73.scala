import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub73 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((v1, v2))) = {
    (v1, v2) match {
      case (ZERO(c1), ZERO(c2)) | (MONE(c1), ONE(c2)) | (ONE(c1), MONE(c2)) => {
        ZERO(crazy2add(c1, c2))
      }
      case (ZERO(c1), MONE(c2)) | (MONE(c1), ZERO(c2)) => {
        MONE(crazy2add(c1, c2))
      }
      case (ZERO(c1), ONE(c2)) | (ONE(c1), ZERO(c2)) => { ONE(crazy2add(c1, c2))
      }
      case (ONE(c1), ONE(c2)) => { ZERO(crazy2add(ONE(NIL), crazy2add(c1, c2)))
      }
      case (MONE(c1), MONE(c2)) => {
        ZERO(crazy2add(MONE(NIL), crazy2add(c1, c2)))
      }
      case (NIL, c2) => { c2 }
      case (c1, NIL) => { c1 }
    }
  }
  
}
