import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub202 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((x: Crazy2, y: Crazy2))): Crazy2 = {
    x match {
      case NIL => { y }
      case ZERO(x_0) => {
        y match {
          case NIL => { x }
          case ZERO(y_0) => { ZERO(crazy2add(x_0, y_0)) }
          case ONE(y_0) => { ONE(crazy2add(x_0, y_0)) }
          case MONE(y_0) => { MONE(crazy2add(x_0, y_0)) }
        }
      }
      case ONE(x_0) => {
        y match {
          case NIL => { x }
          case ZERO(y_0) => { ONE(crazy2add(x_0, y_0)) }
          case ONE(y_0) => { ZERO(crazy2add(crazy2add(x_0, ONE(NIL)), y_0)) }
          case MONE(y_0) => { ZERO(crazy2add(x_0, y_0)) }
        }
      }
      case MONE(x_0) => {
        y match {
          case NIL => { x }
          case ZERO(y_0) => { MONE(crazy2add(x_0, y_0)) }
          case ONE(y_0) => { ZERO(crazy2add(x_0, y_0)) }
          case MONE(y_0) => { ZERO(crazy2add(crazy2add(x_0, MONE(NIL)), y_0)) }
        }
      }
    }
  }
}