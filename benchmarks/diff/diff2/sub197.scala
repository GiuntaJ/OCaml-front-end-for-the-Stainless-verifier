import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub197 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def make_sum(e: Aexp): Aexp = { Sum(List(e) ++ List(Const(0))) }
  
  def f(((a, x))) = {
    a match {
      case Const(n) => { Const(0) }
      case Var(y) => { if (x == y) Const(1) else Const(0) }
      case Power(y, n) => {
        if (x == y) Times(List(Const(n), Power(y, n - 1))) else Const(0)
      }
      case Times(l) => {
        l match {
          case Nil() => { Const(0) }
          case Cons(hd, tl) => {
            Sum(List(Times(f(hd, x) :: tl), Times(List(hd, f(Times(tl), x)))))
          }
        }
      }
      case Sum(l) => {
        l match {
          case Nil() => { Const(0) }
          case Cons(hd, tl) => { Sum(List(f(hd, x), f(Sum(tl), x))) }
        }
      }
    }
  }
  
  
  val diff: (Aexp, String) => Aexp = {
    case (e, x) => { f(e, x) }
  }
}