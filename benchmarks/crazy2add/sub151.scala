import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub151 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((n1, n2))) = {
    val _2 = {
      def func(((n1, n2, p))) = {
        (n1, n2, p) match {
          case (NIL, NIL, _) => { p }
          case (NIL, _, NIL) => { n2 }
          case (NIL, ZERO(c), ONE(NIL)) => { ONE(c) }
          case (NIL, ZERO(c), MONE(NIL)) => { MONE(c) }
          case (NIL, ONE(c), ONE(NIL)) => { ZERO(func(n1, c, ONE(NIL))) }
          case (NIL, ONE(c), MONE(NIL)) => { ZERO(c) }
          case (NIL, MONE(c), ONE(NIL)) => { ZERO(c) }
          case (NIL, MONE(c), MONE(NIL)) => { ZERO(func(n1, c, MONE(NIL))) }
          case (_, NIL, NIL) => { n1 }
          case (ZERO(c), NIL, ONE(NIL)) => { ONE(c) }
          case (ZERO(c), NIL, MONE(NIL)) => { MONE(c) }
          case (ONE(c), NIL, ONE(NIL)) => { ZERO(func(c, n2, ONE(NIL))) }
          case (ONE(c), NIL, MONE(NIL)) => { ZERO(c) }
          case (MONE(c), NIL, ONE(NIL)) => { ZERO(c) }
          case (MONE(c), NIL, MONE(NIL)) => { ZERO(func(c, n2, MONE(NIL))) }
          case (ZERO(c1), ZERO(c2), NIL) => { ZERO(func(c1, c2, NIL)) }
          case (ZERO(c1), ZERO(c2), ONE(NIL)) => { ONE(func(c1, c2, NIL)) }
          case (ZERO(c1), ZERO(c2), MONE(NIL)) => { MONE(func(c1, c2, NIL)) }
          case (ZERO(c1), ONE(c2), NIL) => { ONE(func(c1, c2, NIL)) }
          case (ZERO(c1), ONE(c2), ONE(NIL)) => { ZERO(func(c1, c2, ONE(NIL))) }
          case (ZERO(c1), ONE(c2), MONE(NIL)) => { ZERO(func(c1, c2, NIL)) }
          case (ZERO(c1), MONE(c2), NIL) => { MONE(func(c1, c2, NIL)) }
          case (ZERO(c1), MONE(c2), ONE(NIL)) => { ZERO(func(c1, c2, NIL)) }
          case (ZERO(c1), MONE(c2), MONE(NIL)) => {
            ZERO(func(c1, c2, MONE(NIL)))
          }
          case (ONE(c1), ZERO(c2), NIL) => { ONE(func(c1, c2, NIL)) }
          case (ONE(c1), ZERO(c2), ONE(NIL)) => { ZERO(func(c1, c2, ONE(NIL))) }
          case (ONE(c1), ZERO(c2), MONE(NIL)) => { ZERO(func(c1, c2, NIL)) }
          case (ONE(c1), ONE(c2), NIL) => { ZERO(func(c1, c2, ONE(NIL))) }
          case (ONE(c1), ONE(c2), ONE(NIL)) => { ONE(func(c1, c2, ONE(NIL))) }
          case (ONE(c1), ONE(c2), MONE(NIL)) => { ONE(func(c1, c2, NIL)) }
          case (ONE(c1), MONE(c2), NIL) => { ZERO(func(c1, c2, NIL)) }
          case (ONE(c1), MONE(c2), ONE(NIL)) => { ONE(func(c1, c2, ONE(NIL))) }
          case (ONE(c1), MONE(c2), MONE(NIL)) => { MONE(func(c1, c2, NIL)) }
          case (MONE(c1), ZERO(c2), NIL) => { MONE(func(c1, c2, NIL)) }
          case (MONE(c1), ZERO(c2), ONE(NIL)) => { ZERO(func(c1, c2, NIL)) }
          case (MONE(c1), ZERO(c2), MONE(NIL)) => {
            ZERO(func(c1, c2, MONE(NIL)))
          }
          case (MONE(c1), ONE(c2), NIL) => { ZERO(func(c1, c2, NIL)) }
          case (MONE(c1), ONE(c2), ONE(NIL)) => { ONE(func(c1, c2, NIL)) }
          case (MONE(c1), ONE(c2), MONE(NIL)) => { MONE(func(c1, c2, NIL)) }
          case (MONE(c1), MONE(c2), NIL) => { ZERO(func(c1, c2, MONE(NIL))) }
          case (MONE(c1), MONE(c2), ONE(NIL)) => { MONE(func(c1, c2, NIL)) }
          case (MONE(c1), MONE(c2), MONE(NIL)) => {
            MONE(func(c1, c2, MONE(NIL)))
          }
        }
      }
      func(n1, n2, NIL)
    }
  }
}