import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub14 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((f, x))) = {
    val _2 = {
      def t_diff(((i, n, l))) = {
        l match {
          case Cons(h, rm) => {
            
              if (
                i == n
              ) {
                TIMES(List(diff(h, x)) ++(rm)) 
              } else {
                SUM(
                  List(TIMES(List(diff(h, x)) ++(rm)),
                   t_diff(i + 1, n, rm ++(List(h)))))
              }
          }
        }
      }
      f match {
        case CONST(i) => { CONST(0) }
        case VAR(v) => { if (v == x) CONST(1) else CONST(0) }
        case POWER(v, n) => {
          
            if (
              n == 0 || v ne x
            ) {
              CONST(0) 
            } else {
              TIMES(List(CONST(n), POWER(v, n - 1)))
            }
        }
        case TIMES(l) => {
          
            if (
              l.length == 0
            ) {
              invalid_arg("Empty TIMES") 
            } else if (
              l.length == 1
            ) {
              diff(l.head, x) 
            } else {
              t_diff(1, l.length, l)
            }
        }
        case SUM(l) => {
          l match {
            case Nil() => { CONST(0) }
            case Cons(a, Nil()) => { diff(a, x) }
            case Cons(h, rm) => { SUM(List(diff(h, x), diff(SUM(rm), x))) }
          }
        }
      }
    }
  }
}