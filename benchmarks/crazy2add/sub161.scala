import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub161 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1, c2))) = {
    (c1, c2) match {
      case (NIL, _) => { c2 }
      case (_, NIL) => { c1 }
      case (ZERO(n1), ZERO(n2)) | (ONE(n1), MONE(n2)) | (MONE(n1), ONE(n2)) => {
        ZERO(crazy2add(n1, n2))
      }
      case (ZERO(n1), ONE(n2)) | (ONE(n1), ZERO(n2)) => { ONE(crazy2add(n1, n2))
      }
      case (ZERO(n1), MONE(n2)) | (MONE(n1), ZERO(n2)) => {
        MONE(crazy2add(n1, n2))
      }
      case (ONE(n1), ONE(n2)) => { ZERO(crazy2add(crazy2add(n1, ONE(NIL)), n2))
      }
      case (MONE(n1), MONE(n2)) => {
        ZERO(crazy2add(crazy2add(n1, MONE(NIL)), n2))
      }
    }
  }
}