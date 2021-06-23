import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub18 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = ( (lam) => { true } )
  
  def lambda(ex: Lambda, l: List[Var]): Boolean = {
    ex match {
      case V(x) => { if (l.contains(x)) true else false }
      case P(x, e) => { lambda(e, l ++ List(x)) }
      case C(e1, e2) => { lambda(e1, l) && lambda(e2, l) }
    }
  }
  
  def check(ex: Lambda): Boolean = { lambda(ex, Nil()) }
}