import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub94 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def findvariable: (Var, List[Var]) => Boolean = {
    case (v, l) =>
      {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == v) true else findvariable(v, tl) }
        }
    }
  }
  
    def checkcheck: (Exp, List[Var]) => Boolean = {
    case (exp, l) =>
      {
        exp match {
          case V(a) => { findvariable(a, l) }
          case P(a, b) => { checkcheck(b, l ++ List(a)) }
          case C(a, b) => { checkcheck(a, l) && checkcheck(b, l) }
        }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { checkcheck(exp, Nil()) } )
}