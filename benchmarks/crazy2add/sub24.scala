import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub24 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /* using test */
  /*
  let rec crazy2val (cv: crazy2): int =
  	match cv with
  	| NIL -> 0
  	| ZERO cv' -> 0 + (2 * crazy2val cv')
  	| ONE cv' -> 1 + (2 * crazy2val cv')
  	| MONE cv' -> -1 + (2 * crazy2val cv')
  */
  
  
  def crazy2add(((cv1: Crazy2, cv2: Crazy2))): Crazy2 = {
    (cv1, cv2) match {
      case (NIL, _) => { cv2 }
      case (_, NIL) => { cv1 }
      case (ZERO(cv1_0), ZERO(cv2_0)) => { ZERO(crazy2add(cv1_0, cv2)) }
      case (ZERO(cv1_0), ONE(cv2_0)) => { ONE(crazy2add(cv1_0, cv2)) }
      case (ZERO(cv1_0), MONE(cv2_0)) => { MONE(crazy2add(cv1_0, cv2)) }
      case (ONE(cv1_0), ZERO(cv2_0)) => { ONE(crazy2add(cv1_0, cv2)) }
      case (ONE(cv1_0), ONE(cv2_0)) => {
        ZERO(crazy2add(ONE(NIL), crazy2add(cv1_0, cv2)))
      }
      case (ONE(cv1_0), MONE(cv2_0)) => { ZERO(crazy2add(cv1_0, cv2)) }
      case (MONE(cv1_0), ZERO(cv2_0)) => { MONE(crazy2add(cv1_0, cv2)) }
      case (MONE(cv1_0), ONE(cv2_0)) => { ZERO(crazy2add(cv1_0, cv2)) }
      case (MONE(cv1_0), MONE(cv2_0)) => {
        ZERO(crazy2add(MONE(NIL), crazy2add(cv1_0, cv2)))
      }
    }
  }
}