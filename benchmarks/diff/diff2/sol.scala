import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sol {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def map: ((Aexp, String) => Aexp, (List[Aexp], String)) => List[Aexp] = {
    case (f, (l, x)) =>
      {
        l match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => { f(hd, x) :: map(f, tl, x) }
        }
    }
  }
  
  def diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        e match {
          case Const(n) => { Const(0) }
          case Var(a) => { if (a != x) Const(0) else Const(1) }
          case Power(a, n) => {
            
              if (
                n < 0
              ) {
                assert(false, "Failure with Invalid ") 
              } else if (
                n == 0 || a != x
              ) {
                Const(0) 
              } else {
                Times(List(Const(n), Power(a, n - 1)))
              }
          }
          case Times(l) => {
            l match {
              case Nil() => { assert(false, "Failure with Invalid") }
              case Cons(hd, Nil()) => { diff(hd, x) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(diff(hd, x) :: tl),
                   Times(List(hd, diff(Times(tl), x)))))
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { assert(false, "Failure with Invalid") }
              case _ => { Sum(map(diff, l, x)) }
            }
          }
        }
    }
  }
}