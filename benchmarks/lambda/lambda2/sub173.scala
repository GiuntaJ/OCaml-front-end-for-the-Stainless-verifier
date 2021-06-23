import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub173 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def lst_check: (String, List[String]) => Boolean = {
    case (var0, env) =>
      {
        env match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == var0) true else lst_check(var0, tl) }
        }
    }
  } 
  
    def well: (Exp, List[String]) => Boolean = {
    case (exp, env) =>
      {
        exp match {
          case V(var0) => { lst_check(var0, env) }
          case P(var0, e) => { well(e, var0 :: env) }
          case C(e1, e2) => { well(e1, env) && well(e2, env) }
        }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { well(exp, Nil()) } )
}