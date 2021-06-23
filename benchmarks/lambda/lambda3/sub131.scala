import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub131 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def find_stack(v, stack) = {
    stack match {
      case Cons(hd, tl) => { if (hd == v) true else find_stack(v, tl) }
      case Nil() => { false }
    }
  }
    
  def check_inner(lam, stack) = {
    lam match {
      case V(v) => { if (find_stack(v, stack)) true else false }
      case P(v, l) => { check_inner(l, v :: stack) }
      case C(l1, l2) => { check_inner(l1, stack) && check_inner(l2, stack) }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { check_inner(lam, Nil()) } )
}