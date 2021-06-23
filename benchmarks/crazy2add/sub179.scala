import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub179 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = (
    (a) =>
      {
        a match {
          case (NIL, NIL) => { NIL }
          case (NIL, n) => { n }
          case (n, NIL) => { n }
          case (ZERO(n1), ZERO(n2)) => { ZERO(crazy2add(n1, n2)) }
          case (ZERO(n1), ONE(n2)) => { ONE(crazy2add(n1, n2)) }
          case (ZERO(n1), MONE(n2)) => { MONE(crazy2add(n1, n2)) }
          case (ONE(n1), ZERO(n2)) => { ONE(crazy2add(n1, n2)) }
          case (ONE(n1), ONE(n2)) => {
            ZERO(crazy2add(crazy2add(n1, n2), ONE(NIL)))
          }
          case (ONE(n1), MONE(n2)) => { ZERO(crazy2add(n1, n2)) }
          case (MONE(n1), ZERO(n2)) => { MONE(crazy2add(n1, n2)) }
          case (MONE(n1), ONE(n2)) => { ZERO(crazy2add(n1, n2)) }
          case (MONE(n1), MONE(n2)) => {
            ZERO(crazy2add(crazy2add(n1, n2), MONE(NIL)))
          }
        }
    }
  )
}
