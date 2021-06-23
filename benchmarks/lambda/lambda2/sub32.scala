import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub32 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def compare(v, l) = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == v) true else compare(v, tl) }
    }
  }
  
    def makel(e, l) = {
    e match {
      case V(v) => { compare(v, l) }
      case P(v, exp1) => { makel(exp1, l ++ List(v)) }
      case C(exp2, exp3) => { makel(exp2, l) && makel(exp3, l) }
    }
  }
  
    val check: Exp => Boolean = ( (e) => { makel(e, Nil()) } )
}