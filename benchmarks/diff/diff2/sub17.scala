import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub17 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(n) => { Const(0) }
          case Var(v) => { if (v == x) Const(1) else Const(0) }
          case Power(v, n) => {
            
              if (
                v == x
              ) {
                
                  if (
                    n == 1
                  ) {
                    Var(v) 
                  } else {
                    Times(List(Const(n), Power("x", n - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(l) => {
            l match {
              case Nil() => { Times(Nil()) }
              case Cons(h, t) => {
                (h, t) match {
                  case (_, Nil()) => { diff(h, x) }
                  case (Const(0), _) => { Const(0) }
                  case (_, _) => {
                    Sum(
                      List(Times(diff(h, x) :: t),
                       Times(List(h, diff(Times(t), x)))))
                  }
                }
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(h, t) => {
                
                  if (
                    t == Nil()
                  ) {
                    diff(h, x) 
                  } else {
                    Sum(List(diff(h, x), diff(Sum(t), x)))
                  }
              }
            }
          }
        }
    }
  }
}