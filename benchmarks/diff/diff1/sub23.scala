import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub23 {
  sealed case class Error(param0: String) extends Exception {}
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((ae, str))) = {
    val _2 = {
      def diff_times(orihd, li, s, n) = {
        li match {
          case Cons(hd, tl) => {
            
              if (
                hd == orihd && n == 1
              ) {
                CONST(0) 
              } else {
                SUM(
                  List(TIMES(diff(hd, s) :: tl)) ++(List(diff_times(orihd, tl ++(List(hd)), s, 1))))
              }
          }
        }
      }
      ae match {
        case CONST(_) => { CONST(0) }
        case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
        case POWER(s, i) => {
          if (s == str) TIMES(List(CONST(i), POWER(s, i - 1))) else CONST(0)
        }
        case TIMES(l) => {
          
            if (
              l == Nil()
            ) {
              assert(false, "Error with no times! ") 
            } else {
              diff_times(l.head, l, str, 0)
            }
        }
        case SUM(Cons(hd, tl)) => {
          
            if (
              tl == Nil()
            ) {
              diff(hd, str) 
            } else {
              SUM(List(diff(hd, str)) ++(List(diff(SUM(tl), str))))
            }
        }
      }
    }
  } 
}