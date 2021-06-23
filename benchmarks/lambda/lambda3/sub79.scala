import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub79 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Bindings = List[Var]
  
  /* bindings */
  val empty_bindings: List[A] = Nil()
  def make_binding[A](v: A, b: List[A]): List[A] = { v :: b }
  def bound[A](b: A, bindings: List[A]): Boolean = {
    bindings match {
      case Nil() => { false }
      case Cons(v, tl) => { if (v == b) true else bound(b, tl) }
    }
  }
  
  def checkBindings: (Lambda, Bindings) => Boolean = {
    case (lam, bindings) =>
      {
        lam match {
          case V(v) => { bound(v, bindings) }
          case P(v, lam) => { checkBindings(lam, make_binding(v, bindings)) }
          case C(lam1, lam2) => {
            checkBindings(lam1, bindings) && checkBindings(lam2, bindings)
          }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { checkBindings(lam, empty_bindings) } )
}