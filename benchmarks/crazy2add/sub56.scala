import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub56 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        c1 match {
          case NIL => { c2 }
          case ZERO(c3) => {
            c2 match {
              case NIL => { c1 }
              case ZERO(c4) => { ZERO(crazy2add(c3, c4)) }
              case ONE(c4) => { ONE(crazy2add(c3, c4)) }
              case MONE(c4) => { MONE(crazy2add(c3, c4)) }
            }
          }
          case ONE(c3) => {
            c2 match {
              case NIL => { c1 }
              case ZERO(c4) => { ONE(crazy2add(c3, c4)) }
              case ONE(c4) => { ZERO(crazy2add(crazy2add(c3, c4), ONE(NIL))) }
              case MONE(c4) => { ZERO(crazy2add(c3, c4)) }
            }
          }
          case MONE(c3) => {
            c2 match {
              case NIL => { c1 }
              case ZERO(c4) => { MONE(crazy2add(c3, c4)) }
              case ONE(c4) => { ZERO(crazy2add(c3, c4)) }
              case MONE(c4) => { ZERO(crazy2add(crazy2add(c3, c4), MONE(NIL))) }
            }
          }
        }
    }
  }
}