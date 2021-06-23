import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub112 {
  
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
          case Var(a) => { if (var0 == a) Const(1) else Var(a) }
          case Power(a, b) => {
            
              if (
                var0 == a
              ) {
                Times(List(Const(b), Power(a, b - 1))) 
              } else {
                Power(a, b)
              }
          }
          case Times(a) => {
            a match {
              case Cons(hd, tl) => {
                Sum(
                  List(Times(List(diff(hd, var0)) ++ tl),
                   Times(List(hd, diff(Times(tl), var0)))))
              }
              case Nil() => { Const(0) }
            }
          }
          case Sum(a) => {
            a match {
              case Cons(hd, tl) => {
                Sum(List(diff(hd, var0), diff(Sum(tl), var0)))
              }
              case Nil() => { Const(0) }
            }
          }
        }
    }
  }
}