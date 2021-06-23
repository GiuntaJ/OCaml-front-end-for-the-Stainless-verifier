import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub42 {
  /********************/ 
  /* Problem 4: Diff   */
  /********************/
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
          case Const(n) => { Const(0) }
          case Var(y) => { if (y == x) Const(1) else Const(0) }
          case Power(z, n) => {
            
              if (
                z == x
              ) {
                
                  if (
                    n == 0
                  ) {
                    Const(0) 
                  } else if (
                    n == 1
                  ) {
                    Const(1) 
                  } else {
                    Times(List(Const(n), Power(z, n - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(l1) => {
            l1 match {
              case Nil() => { Const(0) }
              case Cons(h, Nil()) => { diff(h, x) }
              case Cons(h, t) => {
                h match {
                  case Const(n) => { Times(List(Const(n), diff(Times(t), x))) }
                  case _ => {
                    Sum(
                      List(Times(List(diff(h, x), Times(t))),
                       Times(List(h, diff(Times(t), x)))))
                  }
                }
              }
            }
          }
          case Sum(l1) => {
            l1 match {
              case Nil() => { Const(0) }
              case Cons(h, t) => { Sum(List(diff(h, x), diff(Sum(t), x))) }
            }
          }
        }
    }
  }
}