import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub176 {
  
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
          case Const(n) => { Const(0) }
          case Var(str) => { if (str == x) Const(1) else Const(0) }
          case Power(str, n) => {
            
              if (
                n == 0
              ) {
                Const(0) 
              } else if (
                n == 1
              ) {
                Const(1) 
              } else {
                Times(List(Const(n), Power(str, n - 1)))
              }
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, t1) => {
                t1 match {
                  case Nil() => {
                    Sum(
                      List(Times(List(diff(hd, x), Const(1))),
                       Times(List(hd, diff(Times(t1), x)))))
                  }
                  case _ => {
                    Sum(
                      List(Times(List(diff(hd, x), Times(t1))),
                       Times(List(hd, diff(Times(t1), x)))))
                  }
                }
              }
            }
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, t1) => { Sum(List(diff(hd, x), diff(Sum(t1), x))) }
            }
          }
        }
    }
  }
                              
                /* diff (Sum [Power ("x",2); Times [Const 2; Var "x"]; Const 1],"x") ;; */
}