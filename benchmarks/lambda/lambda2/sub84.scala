import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub84 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def isexist: (List[Var], Var) => Boolean = {
    case (vars, v) =>
      {
        vars match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == v) true else isexist(tl, v) }
        }
    }
  }
  
    def chkvars: (Exp, List[Var]) => Boolean = {
    case (exp, vars) =>
      {
        exp match {
          case V(v) => { isexist(vars, v) }
          case P(v, e) => { chkvars(e, v :: vars) }
          case C(V(v), e) => { chkvars(e, v :: vars) }
        }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { chkvars(exp, Nil()) } )
}