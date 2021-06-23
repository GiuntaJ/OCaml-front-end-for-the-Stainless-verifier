import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub134 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def xfind: (List[String], String) => Boolean = {
    case (en, x) =>
      {
        en match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == x) true else xfind(tl, x) || false }
        }
    }
  }
  
    def find: (Exp, List[String]) => Boolean = {
    case (exp, env) =>
      {
        exp match {
          case V(x) => { xfind(env, x) }
          case P(x, y) => { find(y, x :: env) }
          case C(x, y) => { find(x, env) && find(y, env) }
        }
    }
  }
  	
  	def check: Exp => Boolean = ( (exp) => { find(exp, Nil()) } )	
}