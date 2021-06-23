import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub292 {
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
          case Var(y) => { Var(x) }
          case Power(x, b) => {
            
              if (
                b == 2
              ) {
                Times(List(Const(2), Var(x))) 
              } else {
                Times(List(Const(b), Power(x, b - 1)))
              }
          }
          case Times(l) => {
            l match {
              case Cons(h, t) => {
                t match {
                  case Cons(Var(x), Nil()) => { h }
                  case Cons(Power(a, b), Nil()) => {
                    Times(List(h, diff(Power(a, b), a)))
                  }
                  case _ => { assert(false, "Failure with False") }
                }
              }
            }
          }
          case Sum(k) => {
            k match {
              case Cons(hd, tl) => {
                val _2 = {
                  def diffe(a) = {
                    a match {
                      case Cons(h, t) => {
                        t match {
                          case Nil() => { List(diff(h, x)) }
                          case _ => { diff(h, x) :: diffe(t) }
                        }
                      }
                    }
                  }
                  Sum(diffe(k))
                }
              }
            }
          }
        }
    }
  }
      
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}