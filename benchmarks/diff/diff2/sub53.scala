import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub53 {
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
          case Const(i) => { Const(0) }
          case Var(s) => { if (s == x) Const(1) else Var(s) }
          case Power(s, i) => {
            if (s == x) Times(List(Const(i), Power(s, i - 1))) else Power(s, i)
          }
          case Times(Cons(hd, Cons(hd_0, tl))) => {
            Times(List(hd, diff(hd_0, x)))
          }
          case Sum(li) => {
            val _2 = {
              def diffForExpList: List[Aexp] => List[Aexp] = (
                (expList) =>
                  {
                    expList match {
                      case Nil() => { assert(false, "Failure with empty list") }
                      case Cons(one, Nil()) => { List(diff(one, x)) }
                      case Cons(hd, tl) => { diff(hd, x) :: diffForExpList(tl) }
                    }
                }
              )
              Sum(diffForExpList(li))
            }
          }
        }
    }
  }
}