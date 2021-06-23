import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub79 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val check: Exp => Boolean = ( (exp) => { true } )
  
  	def check2(((exp, lis))) = {
    exp match {
      case C(exp1, exp2) => {
        if (check2(exp1, lis) && check2(exp2, lis)) true else false
      }
      case V(vars) => {
        lis match {
          case Cons(hd, tl) => { if (vars == hd) true else check2(V(vars), tl) }
          case Nil() => { false }
        }
      }
      case P(vars, exp2) => { check2(exp2, vars :: lis) }
    }
  }
  	
  
  	val check: Exp => Boolean = ( (exp) => { check2(exp, Nil()) } ) 
}