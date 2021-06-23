import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub88 {
  
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
          case Const(n) => { Const(0) }
          case Var(n) => { if (n == var0) Const(1) else Const(0) }
          case Power(a, b) => {
            
              if (
                a == var0 && b ne 0
              ) {
                Times(List(Const(b), Power(a, b - 1))) 
              } else {
                Const(0)
              }
          }
          case Times(n) => {
            n match {
              case Nil() => { Times(Nil()) }
              case Cons(hd, tl) => {
                
                  if (
                    tl == Nil()
                  ) {
                    diff(hd, var0) 
                  } else {
                    Sum(
                      List(Times(diff(hd, var0) :: tl),
                       Times(List(hd, diff(Times(tl), var0)))))
                  }
              }
            }
          }
          case Sum(n) => {
            n match {
              case Nil() => { Sum(Nil()) }
              case Cons(hd, tl) => {
                
                  if (
                    tl == Nil()
                  ) {
                    diff(hd, var0) 
                  } else {
                    Sum(List(diff(hd, var0), diff(Sum(tl), var0)))
                  }
              }
            }
          }
        }
    }
  }
}