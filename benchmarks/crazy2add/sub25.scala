import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub25 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  def crazy2add(((cv1: Crazy2, cv2: Crazy2))): Crazy2 = {
    (cv1, cv2) match {
      case (NIL, _) => { cv2 }
      case (_, NIL) => { cv1 }
      case (ZERO(cv1_0), ZERO(cv2_0)) => { ZERO(crazy2add(cv1_0, cv2_0)) }
      case (ZERO(cv1_0), ONE(cv2_0)) => { ONE(crazy2add(cv1_0, cv2_0)) }
      case (ZERO(cv1_0), MONE(cv2_0)) => { MONE(crazy2add(cv1_0, cv2_0)) }
      case (ONE(cv1_0), ZERO(cv2_0)) => { ONE(crazy2add(cv1_0, cv2_0)) }
      case (ONE(cv1_0), ONE(cv2_0)) => {
        ZERO(crazy2add(ONE(NIL), crazy2add(cv1_0, cv2_0)))
      }
      case (ONE(cv1_0), MONE(cv2_0)) => { ZERO(crazy2add(cv1_0, cv2_0)) }
      case (MONE(cv1_0), ZERO(cv2_0)) => { MONE(crazy2add(cv1_0, cv2_0)) }
      case (MONE(cv1_0), ONE(cv2_0)) => { ZERO(crazy2add(cv1_0, cv2_0)) }
      case (MONE(cv1_0), MONE(cv2_0)) => {
        ZERO(crazy2add(MONE(NIL), crazy2add(cv1_0, cv2_0)))
      }
    }
  }
}