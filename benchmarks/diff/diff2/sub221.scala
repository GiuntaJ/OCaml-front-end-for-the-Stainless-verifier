import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub221 {
  
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
          case Const(i) => { Times(Nil()) }
          case Var(s) => { Const(1) }
          case Power(s, i) => {
            if (s == x) Times(List(Const(i), Power(s, i - 1))) else Power(s, i)
          }
          case Times(l) => {
            l match {
              case Nil() => { Times(Nil()) }
              case Cons(hd, tl) => {
                hd match {
                  case Const(n) => { hd }
                  case _ => { Times(List(diff(hd, x)) ++ tl) }
                }
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                Sum(List(diff(hd, x)) ++ List(diff(Sum(tl), x)))
              }
            }
          }
        }
    }
  }
}