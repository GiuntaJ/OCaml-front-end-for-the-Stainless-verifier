import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub57 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((add1, add2))) = {
    (add1, add2) match {
      case (l1, NIL) => { l1 }
      case (NIL, l2) => { l2 }
      case (ZERO(l1), ZERO(l2)) => { ZERO(crazy2add(l1, l2)) }
      case (ONE(l1), ZERO(l2)) => { ONE(crazy2add(l1, l2)) }
      case (ZERO(l1), ONE(l2)) => { ONE(crazy2add(l1, l2)) }
      case (ZERO(l1), MONE(l2)) => { MONE(crazy2add(l1, l2)) }
      case (MONE(l1), ZERO(l2)) => { MONE(crazy2add(l1, l2)) }
      case (MONE(l1), ONE(l2)) => { ZERO(crazy2add(l1, l2)) }
      case (ONE(l1), MONE(l2)) => { ZERO(crazy2add(l1, l2)) }
      case (ONE(l1), ONE(l2)) => { crazy2add(ONE(NIL), crazy2add(l1, l2)) }
      case (MONE(l1), MONE(l2)) => { crazy2add(MONE(NIL), crazy2add(l1, l2)) }
    }
  }
  	
}