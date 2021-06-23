import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub55 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def isvarpow: Aexp => Boolean = (
    (aexp) =>
      {
        aexp match {
          case Var(t) => { true }
          case Power(x, t) => { true }
          case _ => { false }
        }
    }
  )
  
  def istherevarpow: List[Aexp] => Boolean = (
    (l) =>
      {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (isvarpow(hd)) true else istherevarpow(tl) }
        }
    }
  )
  
  val aexptolist: Aexp => List[Aexp] = (
    (aexp) =>
      {
        aexp match {
          case Times(Cons(hd, tl)) => { hd :: tl }
          case Sum(Cons(hd, tl)) => { hd :: tl }
          case _ => { Nil() }
        }
    }
  )
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(a) => { Const(0) }
          case Var(t) => { if (t == x) Const(1) else Const(0) }
          case Power(t, n) => {
            if (t == x) Times(List(Const(n), Power(t, n - 1))) else Const(0)
          }
          case Times(Cons(hd, tl)) => {
            Sum(
              Times(diff(hd, x) :: tl) ::
              aexptolist(Times(hd :: aexptolist(diff(Times(tl), x)))))
          }
          case Sum(Cons(hd, tl)) => {
            Sum(diff(hd, x) :: aexptolist(diff(Sum(tl), x)))
          }
        }
    }
  }
}
