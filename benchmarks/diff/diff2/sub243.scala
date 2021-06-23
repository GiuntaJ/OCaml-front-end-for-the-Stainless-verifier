import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub243 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        val _2 = {
          def diff_sum(lst) = {
            lst match {
              case Nil() => { Nil() }
              case Cons(hd, tl) => { diff(hd, x) :: diff_sum(tl) }
            }
          }
          exp match {
            case Const(c) => { Const(0) }
            case Var(s) => { if (s == x) Const(1) else Const(0) }
            case Power(s, i) => {
              
                if (
                  s == x
                ) {
                  Times(List(Const(i), Power(s, i - 1))) 
                } else {
                  Power(s, i)
                }
            }
            case Times(lst) => {
              lst match {
                case Nil() => { Const(0) }
                case Cons(hd, tl) => {
                  Sum(
                    List(Times(diff(hd, x) :: tl),
                     Times(List(hd, diff(Times(tl), x)))))
                }
              }
            }
            case Sum(lst) => {
              lst match {
                case Nil() => { Const(0) }
                case Cons(hd, tl) => { Sum(diff_sum(lst)) }
              }
            }
          }
        }
    }
  }
    
    
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}
