import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub31 {
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
          case Const(_) => { Const(0) }
          case Var(x) => { Const(1) }
          case Power(x, a) => {
            a match {
              case 0 => { Const(0) }
              case 1 => { Const(1) }
              case 2 => { Times(List(Const(2), Var(x))) }
              case _ => { Times(List(Const(a), Power(x, a - 1))) }
            }
          }
          case Times(l) => {
            l match {
              case Nil() => { assert(false, "Failure with NO INPUT") }
              case Cons(b, Nil()) => { diff(b, x) }
              case Cons(h, t) => {
                Sum(
                  List(Times(diff(h, x) :: t),
                   Times(List(h, diff(Times(t), x)))))
              }
            }
          }
          case Sum(s) => {
            s match {
              case Nil() => { assert(false, "Failure with NO INPUT") }
              case Cons(b, Nil()) => { diff(b, x) }
              case Cons(h, t) => { Sum(List(diff(h, x), diff(Sum(t), x))) }
            }
          }
        }
    }
  }
}