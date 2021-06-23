import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub194 {
  /* problem 4 */
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
          case Var(y) => { if (y == x) Const(1) else Const(0) }
          case Power(x, 0) => { Const(0) }
          case Power(y, n) => {
            if (y == x) Times(List(Const(n), Power(x, n - 1))) else Const(0)
          }
          case Times(Cons(Const(n), Cons(Var(y), Nil()))) => {
            if (y == x) Const(n) else Const(0)
          }
          case Times(Cons(Const(k), Cons(Power(y, n), Nil()))) => {
            
              if (
                y == x
              ) {
                Times(List(Const(k), Const(n), Power(x, n - 1))) 
              } else {
                Const(0)
              }
          }
          case Times(Cons(Power(y, p), Cons(Power(x, n), Nil()))) => {
            
              if (
                y == x
              ) {
                Times(List(Const(p + n), Power(x, n + p - 1))) 
              } else {
                Times(List(Const(n), Power(y, p), Power(x, n - 1)))
              }
          }
          case Sum(Cons(Const(n), Cons(Var(y), Nil()))) => {
            if (y == x) Const(1) else Const(0)
          }
          case Sum(Cons(Const(k), Cons(Power(y, n), Nil()))) => {
            if (y == x) Times(List(Const(n), Power(x, n - 1))) else Const(0)
          }
        }
    }
  }
}