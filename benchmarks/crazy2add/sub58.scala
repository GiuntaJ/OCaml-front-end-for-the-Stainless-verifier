import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub58 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        c1 match {
          case NIL => {
            c2 match {
              case NIL => { NIL }
              case ZERO(cc2) => { ZERO(crazy2add(NIL, cc2)) }
              case ONE(cc2) => { ONE(crazy2add(NIL, cc2)) }
              case MONE(cc2) => { MONE(crazy2add(NIL, cc2)) }
            }
          }
          case ZERO(cc1) => {
            c2 match {
              case NIL => { ZERO(crazy2add(cc1, NIL)) }
              case ZERO(cc2) => { ZERO(crazy2add(cc1, cc2)) }
              case ONE(cc2) => { ONE(crazy2add(cc1, cc2)) }
              case MONE(cc2) => { MONE(crazy2add(cc1, cc2)) }
            }
          }
          case ONE(cc1) => {
            c2 match {
              case NIL => { ONE(crazy2add(cc1, NIL)) }
              case ZERO(cc2) => { ONE(crazy2add(cc1, cc2)) }
              case ONE(cc2) => { ZERO(crazy2add(ONE(NIL), crazy2add(cc1, cc2)))
              }
              case MONE(cc2) => { ZERO(crazy2add(cc1, cc2)) }
            }
          }
          case MONE(cc1) => {
            c2 match {
              case NIL => { MONE(crazy2add(cc1, NIL)) }
              case ZERO(cc2) => { MONE(crazy2add(cc1, cc2)) }
              case ONE(cc2) => { ZERO(crazy2add(cc1, cc2)) }
              case MONE(cc2) => {
                ZERO(crazy2add(MONE(NIL), crazy2add(cc1, cc2)))
              }
            }
          }
        }
    }
  }
}