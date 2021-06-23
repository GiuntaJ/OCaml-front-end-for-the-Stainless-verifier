import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub224 {
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
          case Const(c) => { Const(0) }
          case Var(v) => { if (v == x) Const(1) else Var(v) }
          case Power(str, po) => {
            str match {
              case x => { Times(List(Const(po), Power(str, po - 1))) }
            }
          }
          case Times(l) => {
            l match {
              case Nil() => { assert(false, "Failure with wrong format") }
              case Cons(hd, tl) => {
                val _5 = {
                  val inner_hd = tl match {
                    case Cons(hd2, tl2) => { hd2 }
                    case Nil() => { Const(0) }
                  }
                  Sum(
                    List(Times(List(diff(hd, x), inner_hd)),
                     Times(List(hd, diff(inner_hd, x)))))
                }
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { assert(false, "Failure with wrong format") }
              case Cons(hd, tl) => {
                val _2 = {
                  val inner_hd = tl match {
                    case Cons(hd2, tl2) => { hd2 }
                    case Nil() => { Const(0) }
                  }
                  Sum(List(diff(hd, x), diff(inner_hd, x)))
                }
              }
            }
          }
        }
    }
  }
}