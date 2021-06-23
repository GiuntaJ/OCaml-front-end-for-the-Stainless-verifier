import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub53 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def vpc_eval(exp1: Exp, ar_env: List[Var]): Boolean = {
    exp1 match {
      case V(a) => {
        val _2 = {
          def ck(l1, st) = {
            l1 match {
              case Nil() => { false }
              case Cons(h, t) => { if (h == st) true else ck(t, st) }
            }
          }
          if (ck(ar_env, a)) true else false
        }
      }
      case P(v, e1) => { if (vpc_eval(e1, ar_env ++ List(v))) true else false }
      case C(e1, e2) => {
        if (vpc_eval(e1, ar_env) && vpc_eval(e2, ar_env)) true else false
      }
    }
  }
  
  val check: Exp => Boolean = ( (e) => { vpc_eval(e, Nil()) } )
}