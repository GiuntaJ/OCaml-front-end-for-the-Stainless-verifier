import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub143 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def has: (String, Exp) => Boolean = {
    case (str, expression) =>
      {
        expression match {
          case V(v) => { false }
          case P(v, e) => { if (v == str) true else false }
          case C(e1, e2) => { has(str, e1) || has(str, e2) }
        }
    }
  }
  
  	def checkOriginal: (Exp, Exp) => Boolean = {
    case (exp, original) =>
      {
        exp match {
          case V(v) => { has(v, original) }
          case P(v, e) => { checkOriginal(e, original) }
          case C(e1, e2) => {
            checkOriginal(e1, original) && checkOriginal(e2, original)
          }
        }
    }
  }
  
  	def check: Exp => Boolean = ( (exp) => { checkOriginal(exp, exp) } )
}