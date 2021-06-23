import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub14 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def inlist: (String, List[String]) => Boolean = {
    case (a, l) =>
      {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == a) true else inlist(a, tl) }
        }
    }
  }
  
  
  
  	def chlist: (Exp, List[String]) => Boolean = {
    case (e, l) =>
      {
        e match {
          case V(a) => { inlist(a, l) }
          case P(a, env) => { chlist(env, a :: l) }
          case C(env1, env2) => { chlist(env1, l) && chlist(env2, l) }
        }
    }
  }
  
    def check: Exp => Boolean = ( (e) => { chlist(e, Nil()) } )
}