import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub286 {
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
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(var0, num) => {
            
              if (
                var0 == x && num == 1
              ) {
                Const(1) 
              } else if (
                var0 == x && num == 2
              ) {
                Times(List(Const(num), Var(var0))) 
              } else if (
                var0 == x
              ) {
                Times(List(Const(num), Power(var0, num - 1))) 
              } else {
                Const(0)
              }
          }
          case Times(e) => {
            e match {
              case Cons(hd, tl) => {
                
                  if (
                    tl == Nil()
                  ) {
                    diff(hd, x) 
                  } else {
                    Sum(
                      List(Times(diff(hd, x) :: tl),
                       Times(List(hd, diff(Times(tl), x)))))
                  }
              }
              case Nil() => { Times(e) }
            }
          }
          case Sum(e) => {
            e match {
              case Cons(hd, tl) => {
                
                  if (
                    tl == Nil()
                  ) {
                    diff(hd, x) 
                  } else {
                    Sum(List(diff(hd, x), diff(Sum(tl), x)))
                  }
              }
              case Nil() => { Sum(e) }
            }
          }
        }
    }
  }
    	  
}