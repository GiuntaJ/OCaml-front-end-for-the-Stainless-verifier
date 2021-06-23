import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub176 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
      
    def checklist: (List[Var], Var) => Boolean = {
    case (l, v) =>
      {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == v) true else checklist(tl, v) }
        }
    }
  }
  
    def check2: (Exp, List[Var]) => Boolean = {
    case (exp, l) =>
      {
        exp match {
          case V(v) => { if (checklist(l, v)) true else false }
          case P(v, e) => { check2(e, v :: l) }
          case C(e1, e2) => { check2(e1, l) && check2(e2, l) }
        }
    }
  } 
  
  
    val check: Exp => Boolean = ( (exp) => { check2(exp, Nil()) } ) 
}