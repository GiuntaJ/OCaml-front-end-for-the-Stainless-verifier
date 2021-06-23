import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub109 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
  
    val check: Exp => Boolean = ( (e) => { true } )
  
  
    def isFreeVar: (Var, List[String]) => Boolean = {
    case (v, l) =>
      {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (v == hd) true else isFreeVar(v, tl) }
        }
    }
  }
  
    def findVar: (Exp, List[String]) => Boolean = {
    case (e, l) =>
      {
        e match {
          case V(a) => { isFreeVar(a, l) }
          case P(v, e1) => { findVar(e1, l ++ List(v)) }
          case C(e1, e2) => {
            if ((findVar(e1, l) && findVar(e2, l)) == true) true else false
          }
        }
    }
  }
  
  
    val check: Exp => Boolean = ( (e) => { findVar(e, Nil()) } )
}