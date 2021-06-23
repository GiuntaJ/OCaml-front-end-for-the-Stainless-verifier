import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub123 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def elem[A](a: A, l: List[A]): Boolean = {
    l match {
      case Cons(hd, tl) => { a == hd || elem(a, tl) }
      case Nil() => { false }
    }
  }
  
  def _check(lam: Lambda, l: List[Var]): Boolean = {
    lam match {
      case V(v) => { elem(v, l) }
      case P(v, lam_0) => { _check(lam_0, v :: l) }
      case C(lam1, lam2) => { _check(lam1, l) && _check(lam2, l) }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { _check(lam, Nil()) } )
}