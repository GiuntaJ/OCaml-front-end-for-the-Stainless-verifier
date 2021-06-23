import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub44 {
  
  sealed case class Error(param0: String) extends Exception {}
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((ae_exp, str_diff))) = {
    ae_exp match {
      case CONST(i) => { CONST(0) }
      case VAR(str) => { if (str == str_diff) CONST(1) else CONST(0) }
      case POWER(str, i) => {
        
          if (
            str == str_diff
          ) {
            TIMES(List(CONST(i), POWER(str, i - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(list_ae) => {
        list_ae match {
          case Nil() => { assert(false, "Error with Invalid TIMES []") }
          case Cons(hd, Nil()) => { diff(hd, str_diff) }
          case Cons(hd, tl) => {
            SUM(
              List(TIMES(diff(hd, str_diff) :: tl),
               TIMES(List(hd, diff(TIMES(tl), str_diff)))))
          }
        }
      }
      case SUM(list_ae) => {
        list_ae match {
          case Nil() => { assert(false, "Error with Invalid SUM []") }
          case _ => {
            val _2 = {
              def diff_sum(((list_ae_, result))) = {
                list_ae_ match {
                  case Nil() => { result }
                  case Cons(hd, tl) => {
                    diff_sum(tl, result ++(List(diff(hd, str_diff))))
                  }
                }
              }
              SUM(diff_sum(list_ae, Nil()))
            }
          }
        }
      }
    }
  }
  
}
