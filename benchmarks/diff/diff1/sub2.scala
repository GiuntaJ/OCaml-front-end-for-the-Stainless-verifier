import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub2 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def simplify(a: Ae): Ae = {
    a match {
      case POWER(_, 0) => { CONST(1) }
      case TIMES(l) => {
        
          if (
            l.contains(CONST(0))
          ) {
            CONST(0) 
          } else if (
            l.length == 1
          ) {
            simplify(l.apply(1)) 
          } else {
            TIMES(l)
          }
      }
      case SUM(ll) => { SUM(ll.map(simplify)) }
      case _ => { a }
    }
  }
  
  def diff(((a, str))) = {
    a match {
      case CONST(n) => { CONST(0) }
      case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
      case POWER(s, p) => {
        
          if (
            p == 0
          ) {
            CONST(0) 
          } else if (
            s == str
          ) {
            
              if (
                p == 1
              ) {
                CONST(1) 
              } else {
                simplify(TIMES(List(CONST(p), POWER(s, p - 1))))
              } 
          } else {
            CONST(0)
          }
      }
      case TIMES(l) => {
        l match {
          case Nil() => { invalid_arg("invalid value!") }
          case Cons(h, Nil()) => { simplify(diff(h, str)) }
          case Cons(h, t) => {
            simplify(
              SUM(
                List(TIMES(List(diff(h, str), TIMES(t))),
                 TIMES(List(h, diff(TIMES(t), str))))))
          }
        }
      }
      case SUM(ll) => {
        ll match {
          case Nil() => { CONST(0) }
          case Cons(h, Nil()) => { simplify(diff(h, str)) }
          case Cons(h, t) => {
            simplify(SUM(ll.map(( (aa) => { diff(aa, str) } ))))
          }
        }
      }
    }
  }
}
