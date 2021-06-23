import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub260 {
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
          case Const(n) => { Const(0) }
          case Var(y) => { if (x == y) Const(1) else Const(0) }
          case Power(str, n) => {
            
              if (
                str == x
              ) {
                n match {
                  case 0 => { Const(0) }
                  case 1 => { Const(1) }
                  case _ => { Times(List(Const(n), Power(str, n - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(head, tail) => {
                Sum(
                  List(Times(diff(head, x) :: tail),
                   Times(List(head, diff(Times(tail), x)))))
              }
            }
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(head, tail) => {
                Sum(List(diff(head, x), diff(Sum(tail), x)))
              }
            }
          }
        }
    }
  }
      
}