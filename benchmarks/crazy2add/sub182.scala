import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub182 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((a: Crazy2, b: Crazy2))): Crazy2 = {
    a match {
      case NIL => { b }
      case ZERO(atl) => {
        b match {
          case NIL => { a }
          case ZERO(btl) => { ZERO(crazy2add(atl, btl)) }
          case ONE(btl) => { ONE(crazy2add(atl, btl)) }
          case MONE(btl) => { MONE(crazy2add(atl, btl)) }
        }
      }
      case ONE(atl) => {
        b match {
          case NIL => { a }
          case ZERO(btl) => { ONE(crazy2add(atl, btl)) }
          case ONE(btl) => { ZERO(crazy2add(ONE(NIL), crazy2add(atl, btl))) }
          case MONE(btl) => { ZERO(crazy2add(atl, btl)) }
        }
      }
      case MONE(atl) => {
        b match {
          case NIL => { a }
          case ZERO(btl) => { MONE(crazy2add(atl, btl)) }
          case ONE(btl) => { ZERO(crazy2add(atl, btl)) }
          case MONE(btl) => { ZERO(crazy2add(MONE(NIL), crazy2add(atl, btl))) }
        }
      }
    }
  }
}