import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub74 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2val: Crazy2 => Int63 = (
    (a) =>
      {
        a match {
          case NIL => { 0 }
          case ZERO(b) => { 2 * crazy2val(b) }
          case ONE(b) => { 1 + 2 * crazy2val(b) }
          case MONE(b) => { -(1) + 2 * crazy2val(b) }
        }
    }
  )
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        a match {
          case NIL => { b }
          case ZERO(at) => {
            b match {
              case NIL => { a }
              case ZERO(bt) => { ZERO(crazy2add(at, bt)) }
              case ONE(bt) => { ONE(crazy2add(at, bt)) }
              case MONE(bt) => { MONE(crazy2add(at, bt)) }
            }
          }
          case ONE(at) => {
            b match {
              case NIL => { a }
              case ZERO(bt) => { ONE(crazy2add(at, bt)) }
              case ONE(bt) => { ZERO(crazy2add(crazy2add(ONE(NIL), at), bt)) }
              case MONE(bt) => { ZERO(crazy2add(at, bt)) }
            }
          }
          case MONE(at) => {
            b match {
              case NIL => { a }
              case ZERO(bt) => { MONE(crazy2add(at, bt)) }
              case ONE(bt) => { ZERO(crazy2add(at, bt)) }
              case MONE(bt) => { ZERO(crazy2add(crazy2add(MONE(NIL), at), bt)) }
            }
          }
        }
    }
  }
  
}
