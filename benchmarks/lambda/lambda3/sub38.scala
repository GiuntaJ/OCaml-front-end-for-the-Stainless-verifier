import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub38 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val empty_venv: List[A] = Nil()
  def extend_venv[A](x: A, e: List[A]): List[A] = { x :: e }
  def venv_contains[A](e: List[A], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == x) true else venv_contains(tl, x) }
    }
  }
  
  type Venv = List[String]
  
  def check_with_venv: (Lambda, Venv) => Boolean = {
    case (lam, venv) =>
      {
        lam match {
          case V(x) => { venv_contains(venv, x) }
          case P(x, l) => {
            val _2 = {
              val ext_venv = extend_venv(x, venv)
              check_with_venv(l, ext_venv)
            }
          }
          case C(l1, l2) => {
            check_with_venv(l1, venv) && check_with_venv(l2, venv)
          }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { check_with_venv(lam, empty_venv) } )
}