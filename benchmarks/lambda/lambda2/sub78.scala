import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub78 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  
  	def find: (List[Var], Var) => Boolean = {
    case (lst, v) =>
      {
        lst match {
          case Cons(hd, tl) => { if (hd == v) true else find(tl, v) }
          case _ => { false }
        }
    }
  }
  
  	def f: (List[Var], Exp) => Boolean = {
    case (lst, e) =>
      {
        e match {
          case V(v) => { if (find(lst, v)) true else false }
          case P(v, e) => { if (f(lst ++ List(v), e)) true else false }
          case C(e1, e2) => { if (check(e1) && check(e2)) true else false }
        }
    }
  }
  def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(v) => { false }
          case P(v, e) => { if (f(List(v), e)) true else false }
          case C(e1, e2) => { if (check(e1) && check(e2)) true else false }
        }
    }
  )
}