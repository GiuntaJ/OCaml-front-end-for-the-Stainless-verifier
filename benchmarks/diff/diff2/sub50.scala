import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub50 {
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
          case Const(i) => { Const(0) }
          case Var(k) => { if (k == x) Const(1) else Var(k) }
          case Power(k, i) => {
            if (k == x) Times(List(Const(i), Power(k, i - 1))) else Power(k, i)
          }
          case Times(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(h, t) => {
                h match {
                  case Var(k) => {
                    
                      if (
                        k == x
                      ) {
                        Times(diff(h, x) :: t) 
                      } else {
                        Times(List(h, diff(Times(t), x)))
                      }
                  }
                  case Power(k, i) => {
                    
                      if (
                        k == x
                      ) {
                        Times(diff(h, x) :: t) 
                      } else {
                        Times(List(h, diff(Times(t), x)))
                      }
                  }
                  case _ => { Times(List(h, diff(Times(t), x))) }
                }
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(h, t) => { Sum(List(diff(h, x), diff(Sum(t), x))) }
            }
          }
        }
    }
  }
}
