import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub178 {
  /* problem 4 */
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
          case Const(_) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(b, n) => {
            
              if (
                b != x
              ) {
                Const(0) 
              } else {
                n match {
                  case 0 => { Const(0) }
                  case c => { Times(List(Const(c), Power(b, c - 1))) }
                }
              }
          }
          case Times(fx) => {
            fx match {
              case Nil() => { Const(0) }
              case Cons(hd, Nil()) => { diff(hd, x) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(List(diff(hd, x), Times(tl))),
                   Times(List(hd, diff(Times(tl), x)))))
              }
            }
          }
          case Sum(el) => {
            el match {
              case Nil() => { Const(0) }
              case Cons(hd, Nil()) => { diff(hd, x) }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
}