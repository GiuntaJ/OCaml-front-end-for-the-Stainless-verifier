import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub49 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /* crazy2add: crazy2 * crazy2 -> crazy2 */
  def crazy2add(((c1, c2))) = {
    (c1, c2) match {
      case (NIL, _) => { c2 }
      case (_, NIL) => { c1 }
      case (ZERO(ic1), ZERO(ic2)) => { ZERO(crazy2add(ic1, ic2)) }
      case (ZERO(ic1), ONE(ic2)) => { ONE(crazy2add(ic1, ic2)) }
      case (ZERO(ic1), MONE(ic2)) => { MONE(crazy2add(ic1, ic2)) }
      case (ONE(ic1), ZERO(ic2)) => { ONE(crazy2add(ic1, ic2)) }
      case (ONE(ic1), ONE(ic2)) => {
        ZERO(crazy2add(crazy2add(ONE(NIL), ic1), ic2))
      }
      case (ONE(ic1), MONE(ic2)) => { ZERO(crazy2add(ic1, ic2)) }
      case (MONE(ic1), ZERO(ic2)) => { MONE(crazy2add(ic1, ic2)) }
      case (MONE(ic1), ONE(ic2)) => { ZERO(crazy2add(ic1, ic2)) }
      case (MONE(ic1), MONE(ic2)) => {
        ZERO(crazy2add(crazy2add(MONE(NIL), ic1), ic2))
      }
    }
  }
  
  /*
  let rec crazy2val cr2 =
  	match cr2 with
  	| NIL -> 0
  	| ZERO c -> 2 * crazy2val c
  	| ONE c -> 1 + 2 * crazy2val c
  	| MONE c -> -1 + 2 * crazy2val c
  */
}