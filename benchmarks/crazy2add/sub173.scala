import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub173 {
   sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((a: Crazy2, b: Crazy2))): Crazy2 = {
    (a, b) match {
      case (ONE(m), ONE(n)) => { ZERO(crazy2add(ONE(NIL), crazy2add(m, n))) }
      case (ONE(m), ZERO(n)) | (ZERO(m), ONE(n)) => { ONE(crazy2add(m, n)) }
      case (ONE(m), MONE(n)) | (ZERO(m), ZERO(n)) | (MONE(m), ONE(n)) => {
        ZERO(crazy2add(m, n))
      }
      case (MONE(m), ZERO(n)) | (ZERO(m), MONE(n)) => { MONE(crazy2add(m, n)) }
      case (MONE(m), MONE(n)) => { ZERO(crazy2add(MONE(NIL), crazy2add(m, n))) }
      case (NIL, b) => { b }
      case (a, NIL) => { a }
    }
  }			   
}