import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub44 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def equi(x, l) = {
    l match {
      case Nil() => { true }
      case Cons(hd, tl) => { x == hd && equi(x, tl) }
    }
  }
  
    def check_r: (Exp, List[String]) => Boolean = {
    case (e, env) =>
      {
        e match {
          case V(var0) => { if (env == Nil()) false else equi(var0, env) }
          case P(v, e) => { check_r(e, v :: env) }
          case C(e1, e2) => { check_r(e1, env) && check_r(e2, env) }
        }
    }
  }
    
    val check: Exp => Boolean = ( (e) => { check_r(e, Nil()) } )
}