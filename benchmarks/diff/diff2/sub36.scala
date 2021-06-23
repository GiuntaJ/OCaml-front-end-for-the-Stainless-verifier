import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub36 {
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
          case Const(a) => { Const(0) }
          case Var(s) => { diff(Power(s, 1), x) }
          case Power(s, a) => {
            if (s == x) Times(List(Const(a), Power(s, a - 1))) else Const(0)
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(1) }
              case Cons(h, t) => {
                
                  if (
                    t == Nil()
                  ) {
                    diff(h, x) 
                  } else {
                    Sum(
                      List(Times(List(h, diff(Times(t), x))),
                       Times(diff(h, x) :: t)))
                  }
              }
            }
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(h, t) => {
                
                  if (
                    t == Nil()
                  ) {
                    diff(h, x) 
                  } else {
                    Sum(List(diff(h, x), diff(Sum(t), x)))
                  }
              }
            }
          }
        }
    }
  }
}
