import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub84 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((x1, x2))) = {
    x1 match {
      case NIL => { x2 }
      case ZERO(a) => {
        x2 match {
          case NIL => { x1 }
          case ZERO(b) => { ZERO(crazy2add(a, b)) }
          case ONE(b) => { ONE(crazy2add(a, b)) }
          case MONE(b) => { MONE(crazy2add(a, b)) }
        }
      }
      case ONE(a) => {
        x2 match {
          case NIL => { x1 }
          case ZERO(b) => { ONE(crazy2add(a, b)) }
          case ONE(b) => { crazy2add(ZERO(ONE(NIL)), ZERO(crazy2add(a, b))) }
          case MONE(b) => { ZERO(crazy2add(a, b)) }
        }
      }
      case MONE(a) => {
        x2 match {
          case NIL => { x1 }
          case ZERO(b) => { MONE(crazy2add(a, b)) }
          case ONE(b) => { ZERO(crazy2add(a, b)) }
          case MONE(b) => { crazy2add(ZERO(MONE(NIL)), ZERO(crazy2add(a, b))) }
        }
      }
    }
  }
  
}
