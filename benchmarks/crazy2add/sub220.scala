import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub220 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (first_crazy, second_crazy) =>
      {
        first_crazy match {
          case NIL => { second_crazy }
          case ZERO(crazy1_) => {
            second_crazy match {
              case NIL => { first_crazy }
              case ZERO(crazy2_) => { ZERO(crazy2add(crazy1_, crazy2_)) }
              case ONE(crazy2_) => { ONE(crazy2add(crazy1_, crazy2_)) }
              case MONE(crazy2_) => { MONE(crazy2add(crazy1_, crazy2_)) }
            }
          }
          case ONE(crazy1_) => {
            second_crazy match {
              case NIL => { first_crazy }
              case ZERO(crazy2_) => { ONE(crazy2add(crazy1_, crazy2_)) }
              case ONE(crazy2_) => {
                ZERO(crazy2add(ONE(NIL), crazy2add(crazy1_, crazy2_)))
              }
              case MONE(crazy2_) => { ZERO(crazy2add(crazy1_, crazy2_)) }
            }
          }
          case MONE(crazy1_) => {
            second_crazy match {
              case NIL => { first_crazy }
              case ZERO(crazy2_) => { MONE(crazy2add(crazy1_, crazy2_)) }
              case ONE(crazy2_) => { ZERO(crazy2add(crazy1_, crazy2_)) }
              case MONE(crazy2_) => {
                ZERO(crazy2add(MONE(NIL), crazy2add(crazy1_, crazy2_)))
              }
            }
          }
        }
    }
  }
}