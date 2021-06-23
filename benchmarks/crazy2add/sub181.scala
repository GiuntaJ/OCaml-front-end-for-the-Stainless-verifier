import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub181 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  /*
  let rec crazy2val (x: crazy2): int = 
    match x with
    | NIL -> 0
    | ZERO (y) -> (2 * crazy2val (y))
    | ONE (z) -> (1 + 2 * crazy2val (z))
    | MONE (w) -> (-1 + 2 * crazy2val (w))
  */
  
  def crazy2add(((x: Crazy2, y: Crazy2))): Crazy2 = {
    (x, y) match {
      case (NIL, _) => { y }
      case (_, NIL) => { x }
      case (ZERO(a), ZERO(b)) | (MONE(a), ONE(b)) | (ONE(a), MONE(b)) => {
        ZERO(crazy2add(a, b))
      }
      case (ONE(a), ZERO(b)) | (ZERO(a), ONE(b)) => { ONE(crazy2add(a, b)) }
      case (MONE(a), ZERO(b)) | (ZERO(a), MONE(b)) => { MONE(crazy2add(a, b)) }
      case (ONE(a), ONE(b)) => { ZERO(crazy2add(ONE(NIL), crazy2add(a, b))) }
      case (MONE(a), MONE(b)) => { ZERO(crazy2add(MONE(NIL), crazy2add(a, b))) }
    }
  }
    
}