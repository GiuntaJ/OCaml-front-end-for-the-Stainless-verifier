import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub242 {
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
          case Var(y) => {
            if (y == x) Const(1) else Times(List(Var(y), Var("d/dx")))
          }
          case Power(y, a) => {
            
              if (
                y == x
              ) {
                Times(List(Const(a), Power(y, a - 1))) 
              } else {
                Times(List(Const(a), Var("d/dx"), Var(y), Power(y, a - 1)))
              }
          }
          case Sum(lst) => {
            val _5 = {
              def difflst(((lst_0, x_0))) = {
                lst_0 match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => { diff(hd, x_0) :: difflst(tl, x_0) }
                }
              }
              Sum(difflst(lst, x))
            }
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Times(List(Const(0))) }
              case Cons(hd, tl) => {
                val _2 = {
                  def difflst(((lst_0, x_0))) = {
                    lst_0 match {
                      case Nil() => { Nil() }
                      case Cons(hd_0, tl_0) => { diff(hd_0, x_0) :: tl_0 }
                    }
                  }
                  Sum(
                    List(Times(difflst(lst, x)),
                     Times(List(hd, diff(Times(tl), x)))))
                }
              }
            }
          }
        }
    }
  }
        
        
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}