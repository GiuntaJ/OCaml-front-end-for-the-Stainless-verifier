import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub140 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(c: (Crazy2, Crazy2)) = {
    val _2 = {
      def sub(c: (Crazy2, Crazy2), carry: Crazy2) = {
        carry match {
          case ZERO(NIL) => {
            c match {
              case (NIL, NIL) => { NIL }
              case (ZERO(c1), NIL) => { ZERO(sub(c1, NIL, ZERO(NIL))) }
              case (ONE(c1), NIL) => { ONE(sub(c1, NIL, ZERO(NIL))) }
              case (MONE(c1), NIL) => { MONE(sub(c1, NIL, ZERO(NIL))) }
              case (NIL, ZERO(c2)) => { ZERO(sub(NIL, c2, ZERO(NIL))) }
              case (NIL, ONE(c2)) => { ONE(sub(NIL, c2, ZERO(NIL))) }
              case (NIL, MONE(c2)) => { MONE(sub(NIL, c2, ZERO(NIL))) }
              case (ZERO(c1), ZERO(c2)) => { ZERO(sub(c1, c2, ZERO(NIL))) }
              case (ZERO(c1), ONE(c2)) => { ONE(sub(c1, c2, ZERO(NIL))) }
              case (ZERO(c1), MONE(c2)) => { MONE(sub(c1, c2, ZERO(NIL))) }
              case (ONE(c1), ZERO(c2)) => { ONE(sub(c1, c2, ZERO(NIL))) }
              case (ONE(c1), ONE(c2)) => { ZERO(sub(c1, c2, ONE(NIL))) }
              case (ONE(c1), MONE(c2)) => { ZERO(sub(c1, c2, ZERO(NIL))) }
              case (MONE(c1), ZERO(c2)) => { MONE(sub(c1, c2, ZERO(NIL))) }
              case (MONE(c1), ONE(c2)) => { ZERO(sub(c1, c2, ZERO(NIL))) }
              case (MONE(c1), MONE(c2)) => { ZERO(sub(c1, c2, MONE(NIL))) }
            }
          }
          case ONE(NIL) => {
            c match {
              case (NIL, NIL) => { ONE(NIL) }
              case (ZERO(c1), NIL) => { ONE(sub(c1, NIL, ZERO(NIL))) }
              case (ONE(c1), NIL) => { ZERO(sub(c1, NIL, ONE(NIL))) }
              case (MONE(c1), NIL) => { ZERO(sub(c1, NIL, ZERO(NIL))) }
              case (NIL, ZERO(c2)) => { ONE(sub(NIL, c2, ZERO(NIL))) }
              case (NIL, ONE(c2)) => { ZERO(sub(NIL, c2, ONE(NIL))) }
              case (NIL, MONE(c2)) => { ZERO(sub(NIL, c2, ZERO(NIL))) }
              case (ZERO(c1), ZERO(c2)) => { ONE(sub(c1, c2, ZERO(NIL))) }
              case (ZERO(c1), ONE(c2)) => { ZERO(sub(c1, c2, ONE(NIL))) }
              case (ZERO(c1), MONE(c2)) => { ZERO(sub(c1, c2, ZERO(NIL))) }
              case (ONE(c1), ZERO(c2)) => { ZERO(sub(c1, c2, ONE(NIL))) }
              case (ONE(c1), ONE(c2)) => { ONE(sub(c1, c2, ONE(NIL))) }
              case (ONE(c1), MONE(c2)) => { ONE(sub(c1, c2, ZERO(NIL))) }
              case (MONE(c1), ZERO(c2)) => { ZERO(sub(c1, c2, ZERO(NIL))) }
              case (MONE(c1), ONE(c2)) => { ONE(sub(c1, c2, ZERO(NIL))) }
              case (MONE(c1), MONE(c2)) => { MONE(sub(c1, c2, ZERO(NIL))) }
            }
          }
          case MONE(NIL) => {
            c match {
              case (NIL, NIL) => { MONE(NIL) }
              case (ZERO(c1), NIL) => { MONE(sub(c1, NIL, ZERO(NIL))) }
              case (ONE(c1), NIL) => { ZERO(sub(c1, NIL, ZERO(NIL))) }
              case (MONE(c1), NIL) => { ZERO(sub(c1, NIL, MONE(NIL))) }
              case (NIL, ZERO(c2)) => { MONE(sub(NIL, c2, ZERO(NIL))) }
              case (NIL, ONE(c2)) => { ZERO(sub(NIL, c2, ZERO(NIL))) }
              case (NIL, MONE(c2)) => { ZERO(sub(NIL, c2, MONE(NIL))) }
              case (ZERO(c1), ZERO(c2)) => { MONE(sub(c1, c2, ZERO(NIL))) }
              case (ZERO(c1), ONE(c2)) => { ZERO(sub(c1, c2, ZERO(NIL))) }
              case (ZERO(c1), MONE(c2)) => { ZERO(sub(c1, c2, MONE(NIL))) }
              case (ONE(c1), ZERO(c2)) => { ZERO(sub(c1, c2, ZERO(NIL))) }
              case (ONE(c1), ONE(c2)) => { ONE(sub(c1, c2, ZERO(NIL))) }
              case (ONE(c1), MONE(c2)) => { MONE(sub(c1, c2, ZERO(NIL))) }
              case (MONE(c1), ZERO(c2)) => { ZERO(sub(c1, c2, MONE(NIL))) }
              case (MONE(c1), ONE(c2)) => { MONE(sub(c1, c2, ZERO(NIL))) }
              case (MONE(c1), MONE(c2)) => { MONE(sub(c1, c2, MONE(NIL))) }
            }
          }
          case _ => { NIL }
        }
      }
      sub(c, ZERO(NIL))
    }
  }
}