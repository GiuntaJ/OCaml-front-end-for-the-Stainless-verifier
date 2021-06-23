import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub131 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check: Exp => Boolean = (
    (ex) =>
      {
        ex match {
          case _ => { calc(ex, Nil()) }
        }
    }
  )
  def calc: (Exp, List[Var]) => Boolean = {
    case (ex, lst) =>
      {
        ex match {
          case V(a) => { deletion(lst, a) }
          case P(a, b) => { calc(b, lst ++ List(a)) }
          case C(a, b) => {
            if (calc(a, lst) == true && calc(b, lst) == true) true else false
          }
        }
    }
  }
  def deletion: (List[Var], Var) => Boolean = {
    case (lst, va) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (va == hd) true else deletion(tl, va) }
        }
    }
  }
}