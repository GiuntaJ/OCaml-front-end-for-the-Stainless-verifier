import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub74 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Lambda_env = List[Var]
  
  def extend_lenv[A](x: A, lenv: List[A]): List[A] = { x :: lenv }
  
  def l_check: (Lambda, Lambda_env) => Boolean = {
    case (lam, lenv) =>
      {
        lam match {
          case V(x) => {
            lenv match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (x == hd) true else l_check(lam, tl) }
            }
          }
          case P(x, lam) => {
            val _2 = {
              val lenv = extend_lenv(x, lenv)
              l_check(lam, lenv)
            }
          }
          case C(l1, l2) => { l_check(l1, lenv) && l_check(l2, lenv) }
        }
    }
  }
  
  
  
  def check: Lambda => Boolean = ( (lam) => { l_check(lam, Nil()) } )
  
}
