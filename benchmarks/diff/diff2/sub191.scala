import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub191 {
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
          case Var(y) => { if (y == x) Const(1) else Const(0) }
          case Power(z, a) => {
            if (z == x) Times(List(Const(a), Power(z, a - 1))) else Const(0)
          }
          case Sum(sl) => {
            sl match {
              case Nil() => { Sum(Nil()) }
              case Cons(hd, tl) => {
                val _2 = {
                  val newt = diff(Sum(tl), x)
                  newt match {
                    case Sum(ntl) => { Sum(diff(hd, x) :: ntl) }
                    case _ => { Sum(List(diff(hd, x))) }
                  }
                }
              }
            }
          }
          case Times(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(List(diff(hd, x)) ++ tl),
                   Times(List(hd, diff(Times(tl), x)))))
              }
            }
          }
        }
    }
  }
}