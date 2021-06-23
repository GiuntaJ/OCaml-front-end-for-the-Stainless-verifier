import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub70 {
  
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  
  def chk: (Exp, List[String]) => Boolean = {
    case (e, env) =>
      {
        e match {
          case V(a) => { if (findInEnv(a, env)) true else false }
          case P(v, exp) => { chk(exp, v :: env) }
          case C(e1, e2) => { chk(e1, env) && chk(e2, env) }
        }
    }
  }
  def findInEnv: (String, List[String]) => Boolean = {
    case (var0, env) =>
      {
        env match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (var0 == hd) true else findInEnv(var0, tl) }
        }
    }
  }
  
  
  val check: Exp => Boolean = ( (e) => { chk(e, Nil()) } )
}