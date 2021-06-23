import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sol {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def is_mem: (Var, List[Var]) => Boolean = {
    case (x, l) =>
      {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == x) true else is_mem(x, tl) }
        }
    }
  }
  
  def sub_check: (Lambda, List[Var]) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(x) => { is_mem(x, env) }
          case P(x, l) => { sub_check(l, x :: env) }
          case C(l1, l2) => { sub_check(l1, env) && sub_check(l2, env) }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { sub_check(lam, Nil()) } )
}