import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub160 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  
  
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(x) => { if (x == var0) Const(1) else Const(0) }
          case Power(x, integer) => {
            
              if (
                x == var0
              ) {
                Times(List(Const(integer), Power(x, integer - 1))) 
              } else {
                Const(0)
              }
          }
          case Sum(aexplist) => {
            aexplist match {
              case Cons(hd, tl) => {
                
                  if (
                    tl ne Nil()
                  ) {
                    Sum(List(diff(hd, var0), diff(Sum(tl), var0))) 
                  } else {
                    diff(hd, var0)
                  }
              }
            }
          }
          case Times(aexplist) => {
            aexplist match {
              case Cons(hd, tl) => {
                
                  if (
                    tl ne Nil()
                  ) {
                    Sum(
                      List(Times(List(diff(hd, var0), Times(tl))),
                       Times(List(hd, diff(Times(tl), var0))))) 
                  } else {
                    diff(hd, var0)
                  }
              }
            }
          }
        }
    }
  }
}