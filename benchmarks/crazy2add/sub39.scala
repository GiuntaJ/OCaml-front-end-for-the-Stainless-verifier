import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub39 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1, c2))) = {
    val _2 = {
      def crazy2addsub(c1, c2, carry) = {
        (c1, c2, carry) match {
          case (NIL, _, 0) => { c2 }
          case (NIL, _, 1) => { crazy2addsub(ONE(NIL), c2, 0) }
          case (NIL, _, -1) => { crazy2addsub(MONE(NIL), c2, 0) }
          case (_, NIL, 0) => { c1 }
          case (_, NIL, 1) => { crazy2addsub(ONE(NIL), c1, 0) }
          case (_, NIL, -1) => { crazy2addsub(MONE(NIL), c1, 0) }
          case (ZERO(cc1), ZERO(cc2), 0) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), ZERO(cc2), 1) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), ZERO(cc2), -1) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), ONE(cc2), 0) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), ONE(cc2), 1) => { ZERO(crazy2addsub(cc1, cc2, 1)) }
          case (ZERO(cc1), ONE(cc2), -1) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), MONE(cc2), 0) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), MONE(cc2), 1) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), MONE(cc2), -1) => {
            ZERO(crazy2addsub(cc1, cc2, -(1)))
          }
          case (ONE(cc1), ZERO(cc2), 0) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (ONE(cc1), ZERO(cc2), 1) => { ZERO(crazy2addsub(cc1, cc2, 1)) }
          case (ONE(cc1), ZERO(cc2), -1) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (ONE(cc1), ONE(cc2), 0) => { ZERO(crazy2addsub(cc1, cc2, 1)) }
          case (ONE(cc1), ONE(cc2), 1) => { ONE(crazy2addsub(cc1, cc2, 1)) }
          case (ONE(cc1), ONE(cc2), -1) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (ONE(cc1), MONE(cc2), 0) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (ONE(cc1), MONE(cc2), 1) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (ONE(cc1), MONE(cc2), -1) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), ZERO(cc2), 0) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), ZERO(cc2), 1) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), ZERO(cc2), -1) => {
            ZERO(crazy2addsub(cc1, cc2, -(1)))
          }
          case (MONE(cc1), ONE(cc2), 0) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), ONE(cc2), 1) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), ONE(cc2), -1) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), MONE(cc2), 0) => { ZERO(crazy2addsub(cc1, cc2, -(1)))
          }
          case (MONE(cc1), MONE(cc2), 1) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), MONE(cc2), -1) => {
            MONE(crazy2addsub(cc1, cc2, -(1)))
          }
          case _ => { NIL }
        }
      }
      crazy2addsub(c1, c2, 0)
    }
  }
}