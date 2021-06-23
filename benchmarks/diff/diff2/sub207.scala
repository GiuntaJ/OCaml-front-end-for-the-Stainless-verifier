import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub207 {
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
          case Const(y) => { Const(0) }
          case Var(s) => { if (s == x) Const(1) else Var(s) }
          case Times(al) => {
            al match {
              case Cons(hd, tl) => {
                Sum(List(Times(List(diff(hd, x), Sum(tl))), diff(Sum(tl), x)))
              }
            }
          }
          case Power(s, n) => {
            if (s == x) Times(List(Const(n), Power(s, n - 1))) else Power(s, n)
          }
          case Sum(m) => {
            m match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
}