import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub228 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (cry1, cry2) =>
      {
        (cry1, cry2) match {
          case (NIL, _) => { cry2 }
          case (_, NIL) => { cry1 }
          case (ZERO(cry1_), ZERO(cry2_)) => { ZERO(crazy2add(cry1_, cry2_)) }
          case (ZERO(cry1_), ONE(cry2_)) => { ONE(crazy2add(cry1_, cry2_)) }
          case (ZERO(cry1_), MONE(cry2_)) => { MONE(crazy2add(cry1_, cry2_)) }
          case (ONE(cry1_), ZERO(cry2_)) => { ONE(crazy2add(cry1_, cry2_)) }
          case (ONE(cry1_), ONE(cry2_)) => {
            ZERO(crazy2add(ONE(NIL), crazy2add(cry1_, cry2_)))
          }
          case (ONE(cry1_), MONE(cry2_)) => { ZERO(crazy2add(cry1_, cry2_)) }
          case (MONE(cry1_), ZERO(cry2_)) => { MONE(crazy2add(cry1_, cry2_)) }
          case (MONE(cry1_), ONE(cry2_)) => { ZERO(crazy2add(cry1_, cry2_)) }
          case (MONE(cry1_), MONE(cry2_)) => {
            ZERO(crazy2add(MONE(NIL), crazy2add(cry1_, cry2_)))
          }
        }
    }
  }
}