import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub208 {
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
          case Const(integer) => { Const(0) }
          case Var(str) => { if (str == x) Const(1) else Const(0) }
          case Power(str, integer) => {
            
              if (
                integer < 0
              ) {
                Const(0) 
              } else if (
                integer == 1
              ) {
                Const(1) 
              } else {
                Times(List(Const(integer), Power(str, integer - 1)))
              }
          }
          case Times(al1) => {
            al1 match {
              case Nil() => { Const(0) }
              case Cons(aexp1, Nil()) => { diff(aexp1, x) }
              case Cons(h, t) => {
                Sum(
                  List(Times(List(diff(h, x), Times(t))),
                   Times(List(h, diff(Times(t), x)))))
              }
            }
          }
          case Sum(al2) => {
            al2 match {
              case Nil() => { Const(0) }
              case Cons(aexp1, Nil()) => { diff(aexp1, x) }
              case Cons(h, t) => { Sum(List(diff(h, x), diff(Sum(t), x))) }
            }
          }
        }
    }
  }
}