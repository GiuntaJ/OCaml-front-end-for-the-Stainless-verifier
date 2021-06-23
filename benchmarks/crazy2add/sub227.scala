import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub227 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((x: Crazy2, y: Crazy2))): Crazy2 = {
    (x, y) match {
      case (NIL, NIL) => { NIL }
      case (NIL, a) => { a }
      case (a, NIL) => { a }
      case (ZERO(y), ZERO(z)) => { ZERO(crazy2add(y, z)) }
      case (ZERO(y), ONE(z)) => { ONE(crazy2add(y, z)) }
      case (ZERO(y), MONE(z)) => { MONE(crazy2add(y, z)) }
      case (ONE(y), ZERO(z)) => { ONE(crazy2add(y, z)) }
      case (ONE(y), ONE(z)) => { ZERO(crazy2add(ONE(NIL), crazy2add(y, z))) }
      case (ONE(y), MONE(z)) => { ZERO(crazy2add(y, z)) }
      case (MONE(y), ZERO(z)) => { MONE(crazy2add(y, z)) }
      case (MONE(y), ONE(z)) => { ZERO(crazy2add(y, z)) }
      case (MONE(y), MONE(z)) => { ZERO(crazy2add(MONE(NIL), crazy2add(y, z))) }
    }
  }
  
    
}