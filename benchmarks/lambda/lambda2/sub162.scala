import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub162 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def l_match: (String, List[String]) => Boolean = {
    case (v, vlist) =>
      {
        vlist match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == v) true else l_match(v, tl) }
        }
    }
  }
  
  	def v_match: (Exp, List[String]) => Boolean = {
    case (exp, vlist) =>
      {
        exp match {
          case V(v) => { l_match(v, vlist) }
          case P(v, e) => { v_match(e, v :: vlist) }
          case C(e1, e2) => {
            
              if (
                v_match(e1, vlist) == true && v_match(e2, vlist) == true
              ) {
                true 
              } else {
                false
              }
          }
        }
    }
  }
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(v) => { false }
          case P(v, e) => { v_match(e, List(v)) }
          case C(e1, e2) => {
            
              if (
                v_match(e1, Nil()) == true && v_match(e2, Nil()) == true
              ) {
                true 
              } else {
                false
              }
          }
        }
    }
  )
}