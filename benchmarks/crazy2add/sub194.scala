import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub194 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        a match {
          case NIL => { b }
          case ZERO(c) => {
            b match {
              case NIL => { a }
              case ZERO(d) => { ZERO(crazy2add(c, d)) }
              case ONE(d) => { ONE(crazy2add(c, d)) }
              case MONE(d) => { MONE(crazy2add(c, d)) }
            }
          }
          case ONE(c) => {
            b match {
              case NIL => { a }
              case ZERO(d) => { ONE(crazy2add(c, d)) }
              case ONE(d) => { ZERO(crazy2add(crazy2add(c, d), ONE(NIL))) }
              case MONE(d) => { ZERO(crazy2add(c, d)) }
            }
          }
          case MONE(c) => {
            b match {
              case NIL => { a }
              case ZERO(d) => { MONE(crazy2add(c, d)) }
              case ONE(d) => { ZERO(crazy2add(c, d)) }
              case MONE(d) => { ZERO(crazy2add(crazy2add(c, d), MONE(NIL))) }
            }
          }
        }
    }
  }
}