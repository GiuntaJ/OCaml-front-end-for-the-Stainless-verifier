import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub140 {
  
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
          case Var(s) => { if (s == var0) Const(1) else Const(0) }
          case Power(s, a) => {
            
              if (
                s == var0
              ) {
                
                  if (
                    a == 1
                  ) {
                    Const(1) 
                  } else {
                    Times(List(Const(a), Power(s, a - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(t) => {
            t match {
              case Nil() => { Const(0) }
              case Cons(h, t) => {
                
                  if (
                    t == Nil()
                  ) {
                    Sum(
                      List(Times(List(diff(h, var0), Const(1))),
                       Times(List(h, diff(Times(t), var0))))) 
                  } else {
                    Sum(
                      List(Times(List(diff(h, var0), Times(t))),
                       Times(List(h, diff(Times(t), var0)))))
                  }
              }
            }
          }
          case Sum(t2) => {
            t2 match {
              case Nil() => { Const(0) }
              case Cons(h, t) => { Sum(List(diff(h, var0), diff(Sum(t), var0)))
              }
            }
          }
        }
    }
  }
}