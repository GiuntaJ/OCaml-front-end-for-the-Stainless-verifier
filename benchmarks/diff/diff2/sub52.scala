import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub52 {
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
          case Var(b) => { if (b == x) Const(1) else Var(b) }
          case Power(b, a) => {
            if (b == x) Times(List(Const(a), Power(x, a - 1))) else Power(b, a)
          }
          case Times(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                hd match {
                  case Var(b) => {
                    
                      if (
                        b == x
                      ) {
                        Times(diff(hd, x) :: tl) 
                      } else {
                        Times(List(hd, diff(Times(tl), x)))
                      }
                  }
                  case Power(b, a) => {
                    
                      if (
                        b == x
                      ) {
                        Times(diff(hd, x) :: tl) 
                      } else {
                        Times(List(hd, diff(Times(tl), x)))
                      }
                  }
                  case _ => { Times(List(hd, diff(Times(tl), x))) }
                }
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
}