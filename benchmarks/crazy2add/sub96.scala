import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub96 {
  /*
    CSE/2015-21233/김종권
    Homework 2-2
  */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  def crazy2add_0(((c1, c2))) = {
    (c1, c2) match {
      case (NIL, NIL) => { NIL }
      case (NIL, ZERO(z)) => { ZERO(crazy2add_0(NIL, z)) }
      case (NIL, ONE(o)) => { ONE(crazy2add_0(NIL, o)) }
      case (NIL, MONE(m)) => { MONE(crazy2add_0(NIL, m)) }
      case (ZERO(z), NIL) => { ZERO(crazy2add_0(z, NIL)) }
      case (ZERO(z1), ZERO(z2)) => { ZERO(crazy2add_0(z1, z2)) }
      case (ZERO(z), ONE(o)) => { ONE(crazy2add_0(z, o)) }
      case (ZERO(z), MONE(m)) => { MONE(crazy2add_0(z, m)) }
      case (ONE(o), NIL) => { ONE(crazy2add_0(o, NIL)) }
      case (ONE(o), ZERO(z)) => { ONE(crazy2add_0(o, z)) }
      case (ONE(o1), ONE(o2)) => {
        ZERO(crazy2add_0(ONE(NIL), crazy2add_0(o1, o2)))
      }
      case (ONE(o), MONE(m)) => { ZERO(crazy2add_0(o, m)) }
      case (MONE(m), NIL) => { MONE(crazy2add_0(m, NIL)) }
      case (MONE(m), ZERO(z)) => { MONE(crazy2add_0(m, z)) }
      case (MONE(m), ONE(o)) => { ZERO(crazy2add_0(m, o)) }
      case (MONE(m1), MONE(m2)) => {
        ZERO(crazy2add_0(MONE(NIL), crazy2add_0(m1, m2)))
      }
    }
  }
                            
  def crazy2add(((c1, c2))) = { crazy2add_0(c1, c2) }
}