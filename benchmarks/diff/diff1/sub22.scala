import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub22 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((ae, var0))) = {
    ae match {
      case CONST(x) => { CONST(0) }
      case VAR(x) => { CONST(1) }
      case POWER(x, i) => {
        
          if (
            i == 0
          ) {
            CONST(0) 
          } else if (
            i == 1
          ) {
            CONST(1) 
          } else {
            TIMES(List(CONST(i), POWER(x, i - 1)))
          }
      }
      case TIMES(aelist) => {
        aelist match {
          case Nil() => { CONST(0) }
          case Cons(hd, tl) => {
            SUM(
              List(TIMES(diff(hd, var0) :: tl),
               TIMES(List(hd, diff(TIMES(tl), var0)))))
          }
        }
      }
      case SUM(aelist) => { SUM(aelist.map(( (x) => { diff(x, var0) } ))) }
    }
  }
}