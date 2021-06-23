import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub109 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff: (Ae, String) => Ae = {
    case (alexp, dv) =>
      {
        alexp match {
          case CONST(i) => { CONST(0) }
          case VAR(str) => { if (str == dv) CONST(1) else CONST(0) }
          case POWER(str, i) => {
            
              if (
                str == dv
              ) {
                TIMES(List(CONST(i), POWER(str, i - 1))) 
              } else {
                CONST(0)
              }
          }
          case TIMES(Nil()) | SUM(Nil()) => { assert(false, "InvalidArgument") }
          case TIMES(Cons(hd, Nil())) => { diff(hd, dv) }
          case TIMES(Cons(hd, tl)) => {
            SUM(
              List(TIMES(diff(hd, dv) :: tl),
               TIMES(List(hd, diff(TIMES(tl), dv)))))
          }
          case SUM(alexpList) => {
            SUM(alexpList.map(( (alexp) => { diff(alexp, dv) } )))
          }
        }
    }
  }
}