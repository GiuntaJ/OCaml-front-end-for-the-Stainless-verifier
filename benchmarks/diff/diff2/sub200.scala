import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub200 {
  /*problem 4*/
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
          case Const(c) => { Const(0) }
          case Var(v) => { if (v == x) Const(1) else Const(0) }
          case Power(v, c) => {
            if (v == x) Times(List(Const(c), Power(v, c - 1))) else Const(0)
          }
          case Times(Nil()) => { Const(0) }
          case Times(Cons(hd, tl)) => {
            
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
          case Sum(Nil()) => { Const(0) }
          case Sum(Cons(hd, tl)) => {
            
              if (
                tl == Nil()
              ) {
                diff(hd, x) 
              } else {
                Sum(List(diff(hd, x), diff(Sum(tl), x)))
              }
          }
        }
    }
  }
}