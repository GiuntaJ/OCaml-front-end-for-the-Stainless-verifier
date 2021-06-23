import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub100 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        a match {
          case NIL => {
            b match {
              case NIL => { NIL }
              case ONE(c) => { ONE(crazy2add(a, c)) }
              case MONE(c) => { MONE(crazy2add(a, c)) }
              case ZERO(c) => { ZERO(crazy2add(a, c)) }
            }
          }
          case ONE(d) => {
            b match {
              case NIL => { ONE(crazy2add(d, b)) }
              case ONE(c) => { crazy2add(ZERO(crazy2add(d, c)), ZERO(ONE(NIL)))
              }
              case MONE(c) => { ZERO(crazy2add(d, c)) }
              case ZERO(c) => { ONE(crazy2add(d, c)) }
            }
          }
          case MONE(d) => {
            b match {
              case NIL => { MONE(crazy2add(d, b)) }
              case ONE(c) => { ZERO(crazy2add(d, c)) }
              case MONE(c) => {
                crazy2add(ZERO(crazy2add(d, c)), ZERO(MONE(NIL)))
              }
              case ZERO(c) => { MONE(crazy2add(d, c)) }
            }
          }
          case ZERO(d) => {
            b match {
              case NIL => { ZERO(crazy2add(d, b)) }
              case ONE(c) => { ONE(crazy2add(d, c)) }
              case MONE(c) => { MONE(crazy2add(d, c)) }
              case ZERO(c) => { ZERO(crazy2add(d, c)) }
            }
          }
        }
    }
  }
}
