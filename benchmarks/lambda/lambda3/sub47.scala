import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub47 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Venv = List[Var]
  val venv_empty: List[A] = Nil()
  
  def expand_venv[A](x: A, env: List[A]): List[A] = { x :: env }
  def find_venv(x, env) = {
    env match {
      case Nil() => { false }
      case Cons(y, tl) => { if (x == y) true else find_venv(x, tl) }
    }
  }
  
  def check_aux: (Lambda, Venv) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(x) => { find_venv(x, env) }
          case P(x, l) => { check_aux(l, expand_venv(x, env)) }
          case C(l1, l2) => { check_aux(l1, env) && check_aux(l2, env) }
        }
    }
  }
  val check: Lambda => Boolean = ( (lam) => { check_aux(lam, venv_empty) } )
}