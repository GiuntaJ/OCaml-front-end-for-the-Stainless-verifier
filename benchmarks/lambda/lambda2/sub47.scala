import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub47 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def checkbound: (Exp, List[Var]) => Boolean = {
    case (e, l) =>
      {
        e match {
          case V(v) => { l.contains(v) }
          case P(v, e1) => { checkbound(e1, v :: l) }
          case C(e1, e2) => { checkbound(e1, l) && checkbound(e2, l) }
        }
    }
  }
  	
  	def check: Exp => Boolean = ( (e) => { checkbound(e, Nil()) } )
}