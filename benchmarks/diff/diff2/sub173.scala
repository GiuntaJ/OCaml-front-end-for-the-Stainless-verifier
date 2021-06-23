import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub173 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        e match {
          case Const(n) => { Const(0) }
          case Var(y) => { if (y == x) Const(1) else Const(0) }
          case Power(y, i) => {
            if (y == x) Times(List(Const(i), Power(y, i - 1))) else Const(0)
          }
          case Times(Cons(Const(n), Cons(Const(m), Nil()))) => { Const(0) }
          case Times(Cons(Const(n), Cons(Var(y), Nil()))) => {
            if (y == x) Const(n) else Const(0)
          }
          case Times(Cons(Var(y), Cons(Const(n), Nil()))) => {
            if (y == x) Const(n) else Const(0)
          }
          case Times(Cons(Const(n), Cons(Power(y, i), Nil()))) => {
            
              if (
                y == x
              ) {
                Times(List(Const(n), diff(Power(y, i), x))) 
              } else {
                Const(0)
              }
          }
          case Times(Cons(Power(y, i), Cons(Const(n), Nil()))) => {
            
              if (
                y == x
              ) {
                Times(List(Const(n), diff(Power(y, i), x))) 
              } else {
                Const(0)
              }
          }
          case Times(Cons(Const(n), Cons(Times(l), Nil()))) => {
            Times(List(Const(n), diff(Times(l), x)))
          }
          case Times(Cons(Times(l), Cons(Const(n), Nil()))) => {
            Times(List(Const(n), diff(Times(l), x)))
          }
          case Times(Cons(Const(n), Cons(Sum(l), Nil()))) => {
            Times(List(Const(n), diff(Sum(l), x)))
          }
          case Times(Cons(Sum(l), Cons(Const(n), Nil()))) => {
            Times(List(Const(n), diff(Sum(l), x)))
          }
          case Sum(Cons(hd, tl)) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
          case _ => { Const(0) }
        }
    }
  }
}