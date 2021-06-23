import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub79 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((a, b))) = {
    a match {
      case NIL => { b }
      case ZERO(k) => {
        b match {
          case NIL => { a }
          case ZERO(q) => { ZERO(crazy2add(k, q)) }
          case ONE(q) => { ONE(crazy2add(k, q)) }
          case MONE(q) => { MONE(crazy2add(k, q)) }
        }
      }
      case ONE(k) => {
        b match {
          case NIL => { a }
          case ZERO(q) => { ONE(crazy2add(k, q)) }
          case ONE(q) => { ZERO(crazy2add(crazy2add(k, q), ONE(NIL))) }
          case MONE(q) => { ZERO(crazy2add(k, q)) }
        }
      }
      case MONE(k) => {
        b match {
          case NIL => { a }
          case ZERO(q) => { MONE(crazy2add(k, q)) }
          case ONE(q) => { ZERO(crazy2add(k, q)) }
          case MONE(q) => { ZERO(crazy2add(crazy2add(k, q), MONE(NIL))) }
        }
      }
    }
  }
  
}
