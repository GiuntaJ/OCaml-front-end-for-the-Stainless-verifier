import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub106 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = ( (lam) => { check_aux(lam, Nil()) } )
  def check_aux: (Lambda, List[String]) => Boolean = {
    case (lam, li) =>
      {
        lam match {
          case V(x) => { is_include(x, li) }
          case P(x, lmd) => { check_aux(lmd, x :: li) }
          case C(lmd1, lmd2) => { check_aux(lmd1, li) && check_aux(lmd2, li) }
        }
    }
  }
  def is_include: (A, List[A]) => Boolean = {
    case (a, li) =>
      {
        li match {
          case Nil() => { false }
          case Cons(h, t) => { if (a == h) true else is_include(a, t) }
        }
    }
  } 
  	
  
  check(P("a", C(V("a"), P("b", V("a")))))
  
  
  /*
  P ("a", V "a")
  P ("a", P ("a", V "a"))
  P ("a", P ("b", C (V "a", V "b")))
  P ("a", C (V "a", P ("b", V "a")))*/
  
  /*
  P ("a", V "b")
  P ("a", C (V "a", P ("b", V "c")))
  P ("a", P ("b", C (V "a", V "c")))*/
}