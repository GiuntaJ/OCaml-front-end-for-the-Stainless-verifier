import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub63 {
  /* 2 */
  sealed case class InvalidArgument() extends Exception {}
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((exp, str))) = {
    exp match {
      case CONST(c) => { CONST(0) }
      case VAR(x) => { if (x == str) CONST(1) else CONST(0) }
      case POWER(x, p) => {
        
          if (
            p == 0
          ) {
            CONST(0) 
          } else if (
            x == str
          ) {
            TIMES(List(CONST(p), POWER(x, p - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(l) => {
        l match {
          case Nil() => { assert(false, "InvalidArgument") }
          case _ => { SUM(make_times_list(Nil(), l, str)) }
        }
      }
      case SUM(l) => {
        l match {
          case Nil() => { assert(false, "InvalidArgument") }
          case _ => { SUM(make_sum_list(l, str)) }
        }
      }
    }
  }
  def make_times_list(prefix, l, x) = {
    l match {
      case Nil() => { Nil() }
      case Cons(h, t) => {
        val _2 = {
          val d = diff(h, x)
          
            if (
              d == CONST(0)
            ) {
              make_times_list(h :: prefix, t, x) 
            } else {
              TIMES(d :: prefix ++(t)) :: make_times_list(h :: prefix, t, x)
            }
        }
      }
    }
  }
  def make_sum_list(l, x) = {
    l match {
      case Nil() => { Nil() }
      case Cons(h, t) => { diff(h, x) :: make_sum_list(t, x) }
    }
  }
}