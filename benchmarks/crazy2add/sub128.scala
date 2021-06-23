import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub128 {
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazyinc(x: Crazy2): Crazy2 = {
    x match {
      case NIL => { ONE(NIL) }
      case ZERO(z) => { ONE(z) }
      case ONE(z) => { ZERO(crazyinc(z)) }
      case MONE(z) => { ZERO(z) }
    }
  }
  
  def crazydec(x: Crazy2): Crazy2 = {
    x match {
      case NIL => { MONE(NIL) }
      case ZERO(z) => { MONE(z) }
      case ONE(z) => { ZERO(z) }
      case MONE(z) => { ZERO(crazydec(z)) }
    }
  }
  
  def crazy2add(((x, y))) = {
    x match {
      case NIL => { y }
      case ZERO(z) => {
        y match {
          case NIL => { x }
          case ZERO(w) => { ZERO(crazy2add(z, w)) }
          case ONE(w) => { ONE(crazy2add(z, w)) }
          case MONE(w) => { MONE(crazy2add(z, w)) }
        }
      }
      case ONE(z) => {
        y match {
          case NIL => { x }
          case ZERO(w) => { ONE(crazy2add(z, w)) }
          case ONE(w) => { ZERO(crazy2add(crazyinc(z), w)) }
          case MONE(w) => { ZERO(crazy2add(z, w)) }
        }
      }
      case MONE(z) => {
        y match {
          case NIL => { x }
          case ZERO(w) => { MONE(crazy2add(z, w)) }
          case ONE(w) => { ZERO(crazy2add(z, w)) }
          case MONE(w) => { ZERO(crazy2add(crazydec(z), w)) }
        }
      }
    }
  }
}