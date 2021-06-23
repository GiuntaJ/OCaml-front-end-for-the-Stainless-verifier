import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub73 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  
  
  
  
  
  def diff(((ae, var0))) = {
    val _2 = {
      def diff_sub(a) = { diff(a, var0) }
      ae match {
        case CONST(a) => { CONST(0) }
        case VAR(a) => { if (a == var0) CONST(1) else CONST(0) }
        case POWER(a, n) => {
          
            if (
              a == var0
            ) {
              if (n == 1) CONST(1) else TIMES(List(CONST(n), POWER(a, n - 1))) 
            } else {
              CONST(0)
            }
        }
        case TIMES(l) => {
          l match {
            case Nil() => { assert(false, "InvalidArgument") }
            case Cons(hd, Nil()) => { diff(hd, var0) }
            case Cons(hd, tl) => {
              SUM(
                List(TIMES(diff(hd, var0) :: tl)) ++
                List(TIMES(List(hd, diff(TIMES(tl), var0)))))
            }
          }
        }
        case SUM(l) => {
          l match {
            case Nil() => { assert(false, "InvalidArgument") }
            case _ => { SUM(l.map(diff_sub)) }
          }
        }
      }
    }
  }
}