import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub251 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(a) => { Const(0) }
          case Power(a, b) => {
            if (a == x) Times(List(Const(b), Power(a, b - 1))) else Power(a, b)
          }
          case Var(a) => { if (a == x) Const(1) else Var(a) }
          case Sum(lst) => {
            val _5 = {
              def loop(lst) = {
                lst match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => { List(diff(hd, x)) ++ loop(tl) }
                }
              }
              Sum(loop(lst))
            }
          }
          case Times(lst) => {
            val _2 = {
              def loop(lst) = {
                lst match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => {
                    List(hd match {
                       case Times(lst) => { diff(hd, x) }
                       case Sum(lst) => { diff(hd, x) }
                       case Const(a) => { Const(a) }
                       case Var(a) => { diff(hd, x) }
                       case Power(a, b) => { diff(hd, x) }
                     }) ++
                    loop(tl)
                  }
                }
              }
              Times(loop(lst))
            }
          }
        }
    }
  }
}