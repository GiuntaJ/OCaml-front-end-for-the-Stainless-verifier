import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub160 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /* comment crazy2val out before handout */
  def crazy2add(((c2_1, c2_2))) = {
    c2_1 match {
      case NIL => { c2_2 }
      case ZERO(c2_1_pre) => {
        c2_2 match {
          case NIL => { c2_1 }
          case ZERO(c2_2_pre) => { ZERO(crazy2add(c2_1_pre, c2_2_pre)) }
          case ONE(c2_2_pre) => { ONE(crazy2add(c2_1_pre, c2_2_pre)) }
          case MONE(c2_2_pre) => { MONE(crazy2add(c2_1_pre, c2_2_pre)) }
        }
      }
      case ONE(c2_1_pre) => {
        c2_2 match {
          case NIL => { c2_1 }
          case ZERO(c2_2_pre) => { ONE(crazy2add(c2_1_pre, c2_2_pre)) }
          case ONE(c2_2_pre) => {
            ZERO(crazy2add(crazy2add(c2_1_pre, ONE(NIL)), c2_2_pre))
          }
          case MONE(c2_2_pre) => { ZERO(crazy2add(c2_1_pre, c2_2_pre)) }
        }
      }
      case MONE(c2_1_pre) => {
        c2_2 match {
          case NIL => { c2_1 }
          case ZERO(c2_2_pre) => { MONE(crazy2add(c2_1_pre, c2_2_pre)) }
          case ONE(c2_2_pre) => { ZERO(crazy2add(c2_1_pre, c2_2_pre)) }
          case MONE(c2_2_pre) => {
            ZERO(crazy2add(crazy2add(c2_1_pre, MONE(NIL)), c2_2_pre))
          }
        }
      }
    }
  }
}