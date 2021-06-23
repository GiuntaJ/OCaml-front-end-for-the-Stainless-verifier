import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub115 {
  /* 2015-1478 Giyeon Kim HW 2 */
  
  /* Exercise 3 */
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (lcnum, rcnum) =>
      {
        (lcnum, rcnum) match {
          case (NIL, _) => { rcnum }
          case (_, NIL) => { lcnum }
          case (ZERO(t1), ZERO(t2)) => { ZERO(crazy2add(t1, t2)) }
          case (ZERO(t1), ONE(t2)) => { ONE(crazy2add(t1, t2)) }
          case (ZERO(t1), MONE(t2)) => { MONE(crazy2add(t1, t2)) }
          case (ONE(t1), ZERO(t2)) => { ONE(crazy2add(t1, t2)) }
          case (ONE(t1), ONE(t2)) => {
            ZERO(crazy2add(ONE(NIL), crazy2add(t1, t2)))
          }
          case (ONE(t1), MONE(t2)) => { ZERO(crazy2add(t1, t2)) }
          case (MONE(t1), ZERO(t2)) => { MONE(crazy2add(t1, t2)) }
          case (MONE(t1), ONE(t2)) => { ZERO(crazy2add(t1, t2)) }
          case (MONE(t1), MONE(t2)) => {
            ZERO(crazy2add(MONE(NIL), crazy2add(t1, t2)))
          }
        }
    }
  }
  
}
