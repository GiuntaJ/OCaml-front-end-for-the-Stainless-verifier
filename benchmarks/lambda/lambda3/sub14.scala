import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub14 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  /*added function. returns true if given variable is bound, false otherwise*/
  def apply: (A, List[A]) => Boolean = {
    case (var0, env) =>
      {
        env match {
          case Cons(h, t) => { if (var0 == h) true else apply(var0, t) }
          case Nil() => { false }
        }
    }
  }
  
  /*added function. checks all variables in the body of the given lambda expression whether it is bound or not*/
  def bound_var: (Lambda, List[Var]) => Boolean = {
    case (l, env) =>
      {
        l match {
          case V(var0) => { apply(var0, env) }
          case P(var0, lambda) => { bound_var(lambda, var0 :: env) }
          case C(lambda1, lambda2) => {
            bound_var(lambda1, env) && bound_var(lambda2, env)
          }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { bound_var(lam, Nil()) } )
}