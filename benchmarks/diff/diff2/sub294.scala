import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub294 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def map: (A => B, List[A]) => List[B] = {
    case (func, lst) =>
      {
        lst match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => { func(hd) :: map(func, tl) }
        }
    }
  }
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(y) => { if (x != y) Const(0) else Const(1) }
          case Power(y, n) => {
            
              if (
                x != y
              ) {
                Const(0) 
              } else if (
                n < 0
              ) {
                assert(false, "Failure with Invalid Input ") 
              } else if (
                n == 0
              ) {
                Const(0) 
              } else {
                Times(List(Const(n), Power(y, n - 1)))
              }
          }
          case Times(es) => {
            es match {
              case Nil() => { assert(false, "Failure with Invalid Input") }
              case Cons(e, Nil()) => { diff(e, x) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(diff(hd, x) :: tl),
                   Times(List(hd, diff(Times(tl), x)))))
              }
            }
          }
          case Sum(es) => { Sum(map(( (e) => { diff(e, x) } ), es)) }
        }
    }
  }
}