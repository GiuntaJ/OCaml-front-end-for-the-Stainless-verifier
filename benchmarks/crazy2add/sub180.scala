import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub180 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (lc, rc) =>
      {
        (lc, rc) match {
          case (NIL, _) => { rc }
          case (_, NIL) => { lc }
          case (ZERO(lt), ZERO(rt)) => { ZERO(crazy2add(lt, rt)) }
          case (ZERO(lt), ONE(rt)) => { ONE(crazy2add(lt, rt)) }
          case (ZERO(lt), MONE(rt)) => { MONE(crazy2add(lt, rt)) }
          case (ONE(lt), ZERO(rt)) => { ONE(crazy2add(lt, rt)) }
          case (ONE(lt), ONE(rt)) => {
            ZERO(crazy2add(crazy2add(ONE(NIL), lt), rt))
          }
          case (ONE(lt), MONE(rt)) => { ZERO(crazy2add(lt, rt)) }
          case (MONE(lt), ZERO(rt)) => { MONE(crazy2add(lt, rt)) }
          case (MONE(lt), ONE(rt)) => { ZERO(crazy2add(lt, rt)) }
          case (MONE(lt), MONE(rt)) => {
            ZERO(crazy2add(crazy2add(MONE(NIL), lt), rt))
          }
        }
    }
  }
}