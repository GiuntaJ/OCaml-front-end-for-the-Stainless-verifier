import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub35 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  
  def diff(((aexp, x))) = {
    aexp match {
      case Const(a) => { Const(0) }
      case Var(s) => { if (s == x) Const(1) else Const(0) }
      case Power(s, a) => {
        if (a == 1) Const(1) else Times(List(Const(a), Power(s, a - 1)))
      }
      case Times(lst) => {
        lst match {
          case Nil() => { Const(0) }
          case Cons(h, t) => {
            
              if (
                t == Nil()
              ) {
                Sum(
                  List(Times(List(diff(h, x), Const(1))),
                   Times(List(h, diff(Times(t), x))))) 
              } else {
                Sum(
                  List(Times(List(diff(h, x), Times(t))),
                   Times(List(h, diff(Times(t), x)))))
              }
          }
        }
      }
      case Sum(lst2) => {
        lst2 match {
          case Nil() => { Const(0) }
          case Cons(h, t) => { Sum(List(diff(h, x), diff(Sum(t), x))) }
        }
      }
    }
  }			
}