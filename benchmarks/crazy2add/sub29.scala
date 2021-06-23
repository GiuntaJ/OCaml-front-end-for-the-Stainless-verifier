import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub29 {
  /* Department: Electrical and Computer Engineering */
  /* Student ID: 2010-11834 */
  /* Name: Kwonjoon Lee */
  /* Exercise #2 */
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(p: (Crazy2, Crazy2)): Crazy2 = {
    p match {
      case (a, NIL) => { a }
      case (NIL, b) => { b }
      case (ZERO(x), ZERO(y)) => { ZERO(crazy2add(x, y)) }
      case (ZERO(x), ONE(y)) => { ONE(crazy2add(x, y)) }
      case (ZERO(x), MONE(y)) => { MONE(crazy2add(x, y)) }
      case (ONE(x), ZERO(y)) => { ONE(crazy2add(x, y)) }
      case (ONE(x), ONE(y)) => { ZERO(crazy2add(crazy2add(x, y), ONE(NIL))) }
      case (ONE(x), MONE(y)) => { ZERO(crazy2add(x, y)) }
      case (MONE(x), ZERO(y)) => { MONE(crazy2add(x, y)) }
      case (MONE(x), ONE(y)) => { ZERO(crazy2add(x, y)) }
      case (MONE(x), MONE(y)) => { ZERO(crazy2add(crazy2add(x, y), MONE(NIL))) }
    }
  }
}