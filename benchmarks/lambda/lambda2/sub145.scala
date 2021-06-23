import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub145 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	
  	def lcheck: (Exp, List[Var]) => Boolean = {
    case (exp, varList) =>
      {
        exp match {
          case V(var1) => {
            varList match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == var1) true else lcheck(exp, tl) }
            }
          }
          case P(var1, exp1) => { lcheck(exp1, var1 :: varList) }
          case C(exp1, exp2) => { lcheck(exp1, varList) && lcheck(exp2, varList)
          }
        }
    }
  }
  
  	 
  
    val check: Exp => Boolean = ( (exp) => { lcheck(exp, Nil()) } )
}