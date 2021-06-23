import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub32 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((l, r))) = {
    l match {
      case NIL => { r }
      case ZERO(l_) => {
        r match {
          case NIL => { l }
          case ZERO(r_) => { ZERO(crazy2add(l_, r_)) }
          case ONE(r_) => { ONE(crazy2add(l_, r_)) }
          case MONE(r_) => { MONE(crazy2add(l_, r_)) }
        }
      }
      case ONE(l_) => {
        r match {
          case NIL => { l }
          case ZERO(r_) => { ONE(crazy2add(l_, r_)) }
          case ONE(r_) => { ZERO(crazy2add(crazy2add(l_, r_), ONE(NIL))) }
          case MONE(r_) => { ZERO(crazy2add(l_, r_)) }
        }
      }
      case MONE(l_) => {
        r match {
          case NIL => { l }
          case ZERO(r_) => { MONE(crazy2add(l_, r_)) }
          case ONE(r_) => { ZERO(crazy2add(l_, r_)) }
          case MONE(r_) => { ZERO(crazy2add(crazy2add(l_, r_), MONE(NIL))) }
        }
      }
    }
  }
}