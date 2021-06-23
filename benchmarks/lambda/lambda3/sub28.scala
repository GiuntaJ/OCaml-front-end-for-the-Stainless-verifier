import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub28 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def scan[A](x: A, l: List[A]): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (x == hd) true else scan(x, tl) }
    }
  }
  
  def well_formed(exp: Lambda, l: List[Var]): Boolean = {
    exp match {
      case V(x) => { if (scan(x, l)) true else false }
      case P(x, lam) => { well_formed(lam, l ++ List(x)) }
      case C(lam1, lam2) => { well_formed(lam1, l) && well_formed(lam2, l) }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { well_formed(lam, Nil()) } )
  
  
  
  
  
  
  
  
}
