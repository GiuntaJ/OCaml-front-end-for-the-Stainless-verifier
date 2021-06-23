import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub23 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def contain(str, lst) = {
    lst match {
      case Nil() => { false }
      case Cons(h, t) => { if (h == str) true else contain(str, t) }
    }
  }
  
    def chk(expr, env) = {
    expr match {
      case V(str) => { contain(str, env) }
      case P(s, e) => { chk(e, env ++ List(s)) }
      case C(e1, e2) => { chk(e1, env) && chk(e2, env) }
    }
  }
  		
    val check: Exp => Boolean = ( (e) => { chk(e, Nil()) } )
}