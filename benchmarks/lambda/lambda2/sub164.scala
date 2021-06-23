import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub164 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def isin(((str, expr))) = {
    (str, expr) match {
      case (str, V(a)) => { false }
      case (str, P(a, b)) => { if (a == str) true else isin(str, b) }
      case (str, C(a, b)) => { false }
    }
  }
  
  def findvar(exp: Exp, expression: Exp): Boolean = {
    exp match {
      case V(var0) => { isin(var0, expression) }
      case P(var0, expr) => { findvar(expr, expression) || findvar(expr, exp) }
      case C(expr1, expr2) => {
        (findvar(expr1, expression) || findvar(expr1, expr1)) &&
        (findvar(expr2, expression) || findvar(expr2, expr2))
      }
    }
  }
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(var0) => { false }
          case P(var0, expr) => { findvar(expr, exp) }
          case C(expr1, expr2) => { check(expr1) && check(expr2) }
        }
    }
  )
}