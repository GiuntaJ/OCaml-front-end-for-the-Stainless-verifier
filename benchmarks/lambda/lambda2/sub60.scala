import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub60 {
  	
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def check(exp: Exp): Boolean = {
    exp match {
      case V(var0) => { false }
      case _ => { checkExp(exp, Nil()) }
    }
  }
  def checkExp(((exp, env))) = {
    exp match {
      case V(var0) => {
        env match {
          case Nil() => { false }
          case Cons(hd, tail) => { if (hd == var0) true else checkExp(exp, tail)
          }
        }
      }
      case P(var0, exp) => { checkExp(exp, var0 :: env) }
      case C(exp1, exp2) => {
        
          if (
            checkExp(exp1, env) == true
          ) {
            if (checkExp(exp2, env) == true) true else false 
          } else {
            false
          }
      }
    }
  }
}