import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub34 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {} 
  def diff(((aexp, str))) = {
    aexp match {
      case Const(n) => { Const(0) }
      case Var(x) => { if (x == str) Const(1) else aexp }
      case Power(x, n) => {
        if (x == str) Times(List(Const(n), Power(x, n - 1))) else aexp
      }
      case Times(l) => {
        l match {
          case Nil() => { Const(0) }
          case Cons(hd, tl) => {
            Sum(
              List(Times(diff(hd, str) :: tl)) ++
              List(Times(List(hd, diff(Times(tl), str)))))
          }
        }
      }
      case Sum(l) => {
        l match {
          case Nil() => { Const(0) }
          case Cons(hd, tl) => {
            Sum(List(diff(hd, str)) ++ List(diff(Sum(tl), str)))
          }
        }
      }
    }
  } 
  		
}