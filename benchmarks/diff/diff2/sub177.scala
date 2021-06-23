import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub177 {
  /*Problem 4*/
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
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                
                  if (
                    tl == Nil()
                  ) {
                    diff(hd, x) 
                  } else {
                    Sum(List(diff(hd, x)) ++ List(diff(Sum(tl), x)))
                  }
              }
            }
          }
          case Const(n) => { Const(0) }
          case Var(str) => { if (str == x) Const(1) else Const(0) }
          case Power(str, n) => {
            if (str == x) Times(List(Const(n), Power(x, n - 1))) else Const(0)
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => { Times(List(hd) ++ List(diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
}