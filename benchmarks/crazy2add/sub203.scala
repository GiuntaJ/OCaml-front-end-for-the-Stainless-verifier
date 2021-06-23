import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub203 {
  /* Mechanical & Aerospace Eng./2013-11706/Kang Injae/2-3.ml */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /*
  let rec crazy2val : crazy2 -> int = fun v ->
    match v with
    | NIL -> 0
    | ZERO tail -> 2 * (crazy2val tail)
    | ONE tail -> 1 + 2 * (crazy2val tail)
    | MONE tail -> -1 + 2 * (crazy2val tail)
  */
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        (a, b) match {
          case (NIL, _) => { b }
          case (ZERO(m), b) => {
            b match {
              case NIL => { ZERO(m) }
              case ZERO(n) => { ZERO(crazy2add(m, n)) }
              case ONE(n) => { ONE(crazy2add(m, n)) }
              case MONE(n) => { MONE(crazy2add(m, n)) }
            }
          }
          case (ONE(m), b) => {
            b match {
              case NIL => { ONE(m) }
              case ZERO(n) => { ONE(crazy2add(m, n)) }
              case ONE(n) => { ZERO(crazy2add(ONE(NIL), crazy2add(m, n))) }
              case MONE(n) => { ZERO(crazy2add(m, n)) }
            }
          }
          case (MONE(m), b) => {
            b match {
              case NIL => { MONE(m) }
              case ZERO(n) => { MONE(crazy2add(m, n)) }
              case ONE(n) => { ZERO(crazy2add(m, n)) }
              case MONE(n) => { ZERO(crazy2add(MONE(NIL), crazy2add(m, n))) }
            }
          }
        }
    }
  }
}