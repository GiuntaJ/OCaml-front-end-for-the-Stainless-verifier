import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub80 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def p1_help(((aexp, var0))) = {
    aexp match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => { diff(hd, var0) :: p1_help(tl, var0) }
    }
  }
  def diff: (Aexp, String) => Aexp = {
    case (aexp, var0) =>
      {
        aexp match {
          case Const(n) => { Const(0) }
          case Var(y) => { if (y == var0) Const(1) else Const(0) }
          case Power(y, n) => {
            if (y == var0) Times(List(Const(n), Power(y, n - 1))) else Const(0)
          }
          case Times(aexp1) => {
            aexp1 match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(diff(hd, var0) :: tl),
                   Times(hd :: p1_help(tl, var0))))
              }
            }
          }
          case Sum(aexp1) => { Sum(p1_help(aexp1, var0)) }
        }
    }
  }
   
}