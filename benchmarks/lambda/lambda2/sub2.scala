import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub2 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    val check: Exp => Boolean = ( (e) => { true } ) 
  
    def contain(ex, l) = {
    ex match {
      case V(v) => { if (l.contains(v)) true else false }
      case P(v, y) => { contain(y, l ++ List(v)) }
      case C(exp1, exp2) => { contain(exp1, l) && contain(exp2, l) }
    }
  }
  
  def check(ex: Exp): Boolean = { contain(ex, Nil()) }
}