import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub148 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(pair: (Crazy2, Crazy2)): Crazy2 = {
    val _2 = {
      val ((l, r)) = pair
      (l, r) match {
        case (ZERO(n), r) => {
          r match {
            case NIL => { ZERO(n) }
            case ONE(m) => { ONE(crazy2add(n, m)) }
            case ZERO(m) => { ZERO(crazy2add(n, m)) }
            case MONE(m) => { MONE(crazy2add(n, m)) }
          }
        }
        case (ONE(n), r) => {
          r match {
            case NIL => { ONE(n) }
            case ZERO(m) => { ONE(crazy2add(n, m)) }
            case ONE(m) => { ZERO(crazy2add(crazy2add(n, m), ONE(NIL))) }
            case MONE(m) => { ZERO(crazy2add(n, m)) }
          }
        }
        case (MONE(n), r) => {
          r match {
            case NIL => { MONE(n) }
            case ONE(m) => { ZERO(crazy2add(n, m)) }
            case ZERO(m) => { MONE(crazy2add(n, m)) }
            case MONE(m) => { ZERO(crazy2add(crazy2add(n, m), MONE(NIL))) }
          }
        }
        case (l, NIL) => { l }
        case (NIL, r) => { r }
      }
    }
  }
  
}
