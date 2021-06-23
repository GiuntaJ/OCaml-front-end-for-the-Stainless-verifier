import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub8 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((n1, n2))) = {
    n1 match {
      case ZERO(n3) => {
        n2 match {
          case ZERO(n4) => { ZERO(crazy2add(n3, n4)) }
          case ONE(n4) => { ONE(crazy2add(n3, n4)) }
          case MONE(n4) => { MONE(crazy2add(n3, n4)) }
          case NIL => { ZERO(n3) }
        }
      }
      case ONE(n3) => {
        n2 match {
          case ZERO(n4) => { ONE(crazy2add(n3, n4)) }
          case ONE(n4) => { ZERO(crazy2add(crazy2add(n3, ONE(NIL)), n4)) }
          case MONE(n4) => { ZERO(crazy2add(n3, n4)) }
          case NIL => { ONE(n3) }
        }
      }
      case MONE(n3) => {
        n2 match {
          case ZERO(n4) => { MONE(crazy2add(n3, n4)) }
          case ONE(n4) => { ZERO(crazy2add(n3, n4)) }
          case MONE(n4) => { ZERO(crazy2add(crazy2add(n3, MONE(NIL)), n4)) }
          case NIL => { ONE(n3) }
        }
      }
      case NIL => { n2 }
    }
  }
}