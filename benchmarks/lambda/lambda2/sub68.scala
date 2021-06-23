import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub68 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def checklist: (List[String], String) => Boolean = {
    case (l, s) =>
      {
        l match {
          case Nil() => { false }
          case Cons(h, t) => { if (h == s) true else checklist(t, s) }
        }
    }
  }
  
  def extend: (Exp, List[String]) => List[String] = {
    case (x, env) =>
      {
        x match {
          case V(var0) => { if (checklist(env, var0)) env else var0 :: env }
          case P(v, e) => { extend(e, env) }
          case C(e1, e2) => { extend(e1, extend(e2, env)) }
        }
    }
  }
    
    def check: Exp => Boolean = (
    (e) =>
      {
        e match {
          case V(var0) => { true }
          case P(v, e1) => { check(e1) && checklist(extend(e1, Nil()), v) }
          case C(e1, e2) => { check(e1) && check(e2) }
        }
    }
  )
}