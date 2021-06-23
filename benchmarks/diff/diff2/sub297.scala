import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub297 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(s) => { Const(1) }
          case Power(str, pow) => {
            
              if (
                pow == 1
              ) {
                Const(1) 
              } else {
                Times(List(Const(pow), Power(str, pow - 1)))
              }
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                
                  if (
                    tl != Nil()
                  ) {
                    Sum(
                      List(Times(List(diff(hd, x), Times(tl))),
                       Times(List(hd, diff(Times(tl), x))))) 
                  } else {
                    diff(hd, x)
                  }
              }
            }
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                
                  if (
                    tl != Nil()
                  ) {
                    Sum(List(diff(hd, x), diff(Sum(tl), x))) 
                  } else {
                    diff(hd, x)
                  }
              }
            }
          }
        }
    }
  }
}