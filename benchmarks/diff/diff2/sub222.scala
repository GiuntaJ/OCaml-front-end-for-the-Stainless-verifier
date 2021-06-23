import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub222 {
  /*problem4*/
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
          case Const(a) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(a, b) => {
            
              if (
                a == x
              ) {
                
                  if (
                    b != 0
                  ) {
                    Times(List(Const(b), Power(a, b - 1))) 
                  } else {
                    Const(0)
                  } 
              } else {
                Const(0)
              }
          }
          case Times(a) => {
            a match {
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
              case Nil() => { Const(0) }
            }
          }
          case Sum(a) => {
            a match {
              case Cons(hd, tl) => {
                
                  if (
                    tl != Nil()
                  ) {
                    Sum(List(diff(hd, x), diff(Sum(tl), x))) 
                  } else {
                    diff(hd, x)
                  }
              }
              case Nil() => { Const(0) }
            }
          }
        }
    }
  }
}