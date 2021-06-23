import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub167 {
  /* 2012-11230 Kim sangmin */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (x, y) =>
      {
        (x, y) match {
          case (NIL, _) => { y }
          case (_, NIL) => { x }
          case (ZERO(i), ZERO(j)) => { ZERO(crazy2add(i, j)) }
          case (ZERO(i), ONE(j)) => { ONE(crazy2add(i, j)) }
          case (ZERO(i), MONE(j)) => { MONE(crazy2add(i, j)) }
          case (ONE(i), ZERO(j)) => { ONE(crazy2add(i, j)) }
          case (ONE(i), ONE(j)) => { ZERO(crazy2add(ONE(NIL), crazy2add(i, j)))
          }
          case (ONE(i), MONE(j)) => { ZERO(crazy2add(i, j)) }
          case (MONE(i), ZERO(j)) => { MONE(crazy2add(i, j)) }
          case (MONE(i), ONE(j)) => { ZERO(crazy2add(i, j)) }
          case (MONE(i), MONE(j)) => {
            ZERO(crazy2add(MONE(NIL), crazy2add(i, j)))
          }
        }
    }
  }
}
