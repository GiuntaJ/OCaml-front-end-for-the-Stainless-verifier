import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub88 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  	
  def pc(a: Crazy2): Crazy2 = {
    a match {
      case NIL => { ONE(NIL) }
      case ZERO(c) => { ONE(c) }
      case MONE(c) => { ZERO(c) }
      case ONE(c) => { ZERO(pc(c)) }
    }
  }	
  	
  def mc(a: Crazy2): Crazy2 = {
    a match {
      case NIL => { MONE(NIL) }
      case ZERO(c) => { MONE(c) }
      case ONE(c) => { ZERO(c) }
      case MONE(c) => { ZERO(mc(c)) }
    }
  }
  	
  def crazy2add(((a: Crazy2, b: Crazy2))): Crazy2 = {
    (a, b) match {
      case (NIL, NIL) => { NIL }
      case (NIL, _) => { b }
      case (_, NIL) => { a }
      case (ZERO(ca), ZERO(cb)) | (ONE(ca), MONE(cb)) | (MONE(ca), ONE(cb)) => {
        ZERO(crazy2add(ca, cb))
      }
      case (ZERO(ca), ONE(cb)) | (ONE(ca), ZERO(cb)) => { ONE(crazy2add(ca, cb))
      }
      case (ZERO(ca), MONE(cb)) | (MONE(ca), ZERO(cb)) => {
        MONE(crazy2add(ca, cb))
      }
      case (ONE(ca), ONE(cb)) => { ZERO(crazy2add(pc(ca), cb)) }
      case (MONE(ca), MONE(cb)) => { ZERO(crazy2add(ca, mc(cb))) }
    }
  }
}