import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub230 {
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
          case Const(n) => { Const(0) }
          case Var(a) => {
            val _6 = {
              def is_x(a, x) = { if (a == x) Const(1) else Const(0) }
              is_x(a, x)
            }
          }
          case Power(a, n) => {
            
              if (
                a == x
              ) {
                val _3 = {
                  def diff_var(((a, n))) = {
                    Times(List(Const(n), Power(a, n - 1)))
                  }
                  diff_var(a, n)
                } 
              } else {
                Const(0)
              }
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