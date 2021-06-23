import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub20 {
  /*2006-11720 Kim Eunsol HW1 #7*/
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((a, b))) = {
    a match {
      case ZERO(c) => {
        b match {
          case ZERO(d) => { ZERO(crazy2add(c, d)) }
          case ONE(d) => { ONE(crazy2add(c, d)) }
          case MONE(d) => { MONE(crazy2add(c, d)) }
          case NIL => { ZERO(c) }
        }
      }
      case ONE(c) => {
        b match {
          case ZERO(d) => { ONE(crazy2add(c, d)) }
          case ONE(d) => { ZERO(crazy2add(crazy2add(c, ONE(NIL)), d)) }
          case MONE(d) => { ZERO(crazy2add(c, d)) }
          case NIL => { ONE(c) }
        }
      }
      case MONE(c) => {
        b match {
          case ZERO(d) => { MONE(crazy2add(c, d)) }
          case ONE(d) => { ZERO(crazy2add(c, d)) }
          case MONE(d) => { ZERO(crazy2add(crazy2add(c, ONE(NIL)), d)) }
          case NIL => { MONE(c) }
        }
      }
      case NIL => {
        b match {
          case ZERO(d) => { ZERO(d) }
          case ONE(d) => { ONE(d) }
          case MONE(d) => { MONE(d) }
          case NIL => { NIL }
        }
      }
    }
  }
}