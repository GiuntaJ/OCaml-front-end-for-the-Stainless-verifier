import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub6 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def envmt: Exp => List[String] = (
    (e) =>
      {
        val _4 = {
          val lst = Nil()
          e match {
            case V(v) => { lst }
            case P(v, expr) => { v :: envmt(expr) ++ lst }
            case C(exp1, exp2) => { envmt(exp1) ++ envmt(exp2) ++ lst }
          }
        }
    }
  )
  
  def exist: (Exp, List[String]) => Int63 = {
    case (e, lst) =>
      {
        e match {
          case V(v) => {
            lst match {
              case Nil() => { 0 }
              case Cons(hd, tl) => {
                if (hd == v) 1 + exist(e, tl) else exist(e, tl)
              }
            }
          }
          case P(v, exp1) => { exist(exp1, lst) }
          case C(exp1, exp2) => { exist(exp1, lst) + exist(exp2, lst) }
        }
    }
  }
  
  val check: Exp => Boolean = (
    (e) =>
      {
        val _9 = {
          val env = envmt(e)
          
            if (
              exist(e, env) == 0
            ) {
              false 
            } else if (
              env.length > exist(e, env)
            ) {
              false 
            } else {
              true
            }
        }
    }
  )
}