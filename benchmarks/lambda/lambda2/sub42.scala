import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub42 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def isinlist: (List[String], String) => Boolean = {
    case (f, s) =>
      {
        f match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == s) true else isinlist(tl, s) }
        }
    }
  }
  
  def check2: (Exp, List[String]) => Boolean = {
    case (e, state) =>
      {
        e match {
          case V(str) => { isinlist(state, str) }
          case P(str, e) => { check2(e, str :: state) }
          case C(e1, e2) => { check2(e1, state) && check2(e2, state) }
        }
    }
  }
  
  def check: Exp => Boolean = ( (e) => { check2(e, Nil()) } )
}
