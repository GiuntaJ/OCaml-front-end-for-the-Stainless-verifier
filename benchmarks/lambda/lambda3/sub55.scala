import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub55 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def fcheck: (Var, List[Var]) => Boolean = {
    case (v, fl) =>
      {
        fl match {
          case Nil() => { false }
          case Cons(hd, tl) => { hd == v || fcheck(v, tl) }
        }
    }
  }
  
  def check2: (Lambda, List[Var]) => Boolean = {
    case (lam, fl) =>
      {
        lam match {
          case V(v) => { fcheck(v, fl) }
          case P(v, l) => { check2(l, List(v) ++ fl) }
          case C(l1, l2) => { check2(l1, fl) && check2(l2, fl) }
        }
    }
  }
  
  
  def check: Lambda => Boolean = ( (lam) => { check2(lam, Nil()) } )
}