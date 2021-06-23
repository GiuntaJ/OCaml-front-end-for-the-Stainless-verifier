import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub102 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def diff: (Aexp, String) => Aexp = {
    case (aexp, var0) =>
      {
        aexp match {
          case Const(cst1) => { Const(0) }
          case Var(var1) => { if (var1 == var0) Const(1) else Const(0) }
          case Power(str, int1) => {
            int1 match {
              case 0 => { Const(0) }
              case 1 => { if (str == var0) Const(1) else Const(0) }
              case _ => { Times(List(Power(str, int1 - 1), Const(int1))) }
            }
          }
          case Sum(sum_lst) => {
            sum_lst match {
              case Nil() => { Const(0) }
              case Cons(hd, Nil()) => { diff(hd, var0) }
              case Cons(hd, tl) => {
                Sum(List(diff(hd, var0), diff(Sum(tl), var0)))
              }
            }
          }
          case Times(tm_lst) => {
            tm_lst match {
              case Nil() => { Const(1) }
              case Cons(hd, Nil()) => { diff(hd, var0) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(List(diff(hd, var0), Times(tl))),
                   Times(List(hd, diff(Times(tl), var0)))))
              }
            }
          }
        }
    }
  }
}