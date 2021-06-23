import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub90 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  	
  	def checkEnv: (Exp, List[Var]) => Boolean = {
    case (exp, env) =>
      {
        (exp, env) match {
          case (V(_), Nil()) => { false }
          case (V(var0), Cons(hd, tl)) => {
            if (var0 == hd) true else checkEnv(V(var0), tl)
          }
          case (P(var0, exp), env) => { checkEnv(exp, var0 :: env) }
          case (C(e1, e2), env) => { checkEnv(e1, env) && checkEnv(e2, env) }
        }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { checkEnv(exp, Nil()) } )
}