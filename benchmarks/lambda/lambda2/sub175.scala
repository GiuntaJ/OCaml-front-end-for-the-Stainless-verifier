import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub175 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def evaluate(expression, enviornment) = {
            expression match {
              case V(v) => {
                val _7 = {
                  def checker(lst, var0) = {
                    lst match {
                      case Nil() => { false }
                      case Cons(h, t) => {
                        if (h == var0) true else checker(t, var0)
                      }
                    }
                  }
                  if (checker(enviornment, v) == true) true else false
                }
              }
              case P(v, e) => {
                if (evaluate(e, enviornment ++ List(v)) == true) true else false
              }
              case C(e1, e2) => {
                
                  if (
                    evaluate(e1, enviornment) &&
                    evaluate(e2, enviornment) == true
                  ) {
                    true 
                  } else {
                    false
                  }
              }
            }
          }
          evaluate(exp, Nil())
        }
    }
  )
}