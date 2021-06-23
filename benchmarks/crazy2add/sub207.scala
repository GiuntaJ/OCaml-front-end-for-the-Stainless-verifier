import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub207 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  def crazy2add(((crz1, crz2))) = {
    crz1 match {
      case NIL => { crz2 }
      case ZERO(a) => {
        crz2 match {
          case NIL => { crz1 }
          case ZERO(b) => { ZERO(crazy2add(a, b)) }
          case ONE(b) => { ONE(crazy2add(a, b)) }
          case MONE(b) => { MONE(crazy2add(a, b)) }
        }
      }
      case ONE(a) => {
        crz2 match {
          case NIL => { crz1 }
          case ZERO(b) => { ONE(crazy2add(a, b)) }
          case ONE(b) => { ZERO(crazy2add(ONE(NIL), crazy2add(a, b))) }
          case MONE(b) => { ZERO(crazy2add(a, b)) }
        }
      }
      case MONE(a) => {
        crz2 match {
          case NIL => { crz1 }
          case ZERO(b) => { MONE(crazy2add(a, b)) }
          case ONE(b) => { ZERO(crazy2add(a, b)) }
          case MONE(b) => { ZERO(crazy2add(MONE(NIL), crazy2add(a, b))) }
        }
      }
    }
  }
  
}
