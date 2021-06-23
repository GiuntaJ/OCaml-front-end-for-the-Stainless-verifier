import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub4 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    def have(v, env) = {
    env match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == v) true else have(v, tl) }
    }
  }
      
  	def extend(x, env) = { x :: env }
    
    def eval: (Exp, List[Var]) => Boolean = {
    case (e, env) =>
      {
        e match {
          case V(a) => { have(a, env) }
          case P(a, b) => { eval(b, extend(a, env)) }
          case C(a, b) => { if (eval(a, env) && eval(b, env)) true else false }
        }
    }
  }
    
    val check: Exp => Boolean = ( (e) => { eval(e, Nil()) } )
}