import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub195 {
  /*Lee Seok JIn 2013-11417 CSE hw2_3*/
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((cl: Crazy2, cr: Crazy2))): Crazy2 = {
    (cl, cr) match {
      case (_, NIL) => { cl }
      case (NIL, _) => { cr }
      case (ZERO(l), ZERO(r)) => { ZERO(crazy2add(l, r)) }
      case (ZERO(l), ONE(r)) => { ONE(crazy2add(l, r)) }
      case (ZERO(l), MONE(r)) => { MONE(crazy2add(l, r)) }
      case (ONE(l), ZERO(r)) => { ONE(crazy2add(l, r)) }
      case (ONE(l), ONE(r)) => { ZERO(crazy2add(crazy2add(l, r), ONE(NIL))) }
      case (ONE(l), MONE(r)) => { ZERO(crazy2add(l, r)) }
      case (MONE(l), ZERO(r)) => { MONE(crazy2add(l, r)) }
      case (MONE(l), ONE(r)) => { ZERO(crazy2add(l, r)) }
      case (MONE(l), MONE(r)) => { ZERO(crazy2add(crazy2add(l, r), MONE(NIL))) }
    }
  }
}