import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub132 {
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  sealed abstract class Carry {}
  case object Z extends Carry {}
  case object P extends Carry {}
  case object M extends Carry {}
  
  def crazy2addwithcarry(t: (Crazy2, Crazy2, Carry)): Crazy2 = {
    t match {
      case (NIL, x, Z) | (x, NIL, Z) => { x }
      case (NIL, x, P) | (x, NIL, P) => { crazy2addwithcarry(x, ONE(NIL), Z) }
      case (NIL, x, M) | (x, NIL, M) => { crazy2addwithcarry(x, MONE(NIL), Z) }
      case (ZERO(x), ZERO(y), Z) | (ONE(x), MONE(y), Z) | (ONE(x), ZERO(y), M) |
      (ZERO(x), ONE(y), M) | (ZERO(x), MONE(y), P) | (MONE(x), ONE(y), Z) |
      (MONE(x), ZERO(y), P) => {
        ZERO(crazy2addwithcarry(x, y, Z))
      }
      case (ZERO(x), ZERO(y), P) | (ZERO(x), ONE(y), Z) | (ONE(x), ZERO(y), Z) |
      (ONE(x), ONE(y), M) | (ONE(x), MONE(y), P) | (MONE(x), ONE(y), P) => {
        ONE(crazy2addwithcarry(x, y, Z))
      }
      case (ZERO(x), ONE(y), P) | (ONE(x), ZERO(y), P) | (ONE(x), ONE(y), Z) => {
        ZERO(crazy2addwithcarry(x, y, P))
      }
      case (ONE(x), ONE(y), P) => { ONE(crazy2addwithcarry(x, y, P)) }
      case (ZERO(x), ZERO(y), M) | (ZERO(x), MONE(y), Z) |
      (MONE(x), ZERO(y), Z) | (MONE(x), MONE(y), P) | (ONE(x), MONE(y), M) |
      (MONE(x), ONE(y), M) => {
        MONE(crazy2addwithcarry(x, y, Z))
      }
      case (ZERO(x), MONE(y), M) | (MONE(x), ZERO(y), M) | (MONE(x), MONE(y), Z) => {
        ZERO(crazy2addwithcarry(x, y, M))
      }
      case (MONE(x), MONE(y), M) => { MONE(crazy2addwithcarry(x, y, M)) }
    }
  }
  
  def crazy2add(((x, y))) = { crazy2addwithcarry(x, y, Z) }
}