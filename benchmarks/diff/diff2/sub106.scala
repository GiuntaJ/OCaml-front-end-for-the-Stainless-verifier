import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub106 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(a) => { Const(0) }
          case Var(x) => { if (x == var0) Const(1) else Var(x) }
          case Power(x, set) => {
            
              if (
                x == var0
              ) {
                Times(List(Const(set), Power(x, set - 1))) 
              } else {
                Power(x, set)
              }
          }
          case Times(l) => {
            l match {
              case Nil() => { Const(1) }
              case Cons(e, Nil()) => { diff(e, var0) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(diff(hd, var0) :: tl),
                   Times(List(hd, diff(Times(tl), var0)))))
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(e, Nil()) => { diff(e, var0) }
              case Cons(hd, tl) => {
                Sum(List(diff(hd, var0), diff(Sum(tl), var0)))
              }
            }
          }
        }
    }
  } /* TODO */
}