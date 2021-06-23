import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub168 {
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
          case Const(a) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(base, exp) => {
            
              if (
                base == x
              ) {
                Times(List(Const(exp), Power(base, exp - 1))) 
              } else {
                Const(0)
              }
          }
          case Times(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(_, Nil()) => { Const(0) }
              case Cons(th, Cons(tm, tl)) => {
                Sum(
                  List(Times(List(diff(th, x), tm)),
                   Times(List(diff(tm, x), th)), diff(Times(tl), x)))
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(_, Nil()) => { Const(0) }
              case Cons(th, Cons(tm, tl)) => {
                Sum(List(diff(th, x), diff(Sum(tl), x)))
              }
            }
          }
        }
    }
  }
}