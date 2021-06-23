import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub142 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def exist: (Var, List[Var]) => Boolean = {
    case (var0, lst) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == var0) true else exist(var0, tl) }
        }
    }
  }
  
    def checkk: (Exp, List[Var]) => Boolean = {
    case (exp, lst) =>
      {
        exp match {
          case V(v) => { if (exist(v, lst)) true else false }
          case C(e1, e2) => {
            if (checkk(e1, lst) && checkk(e2, lst)) true else false
          }
          case P(v, e) => { checkk(e, v :: lst) }
        }
    }
  }
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(v) => { false }
          case C(e1, e2) => { if (check(e1) && check(e2)) true else false }
          case P(v, e) => { checkk(e, List(v)) }
        }
    }
  )
}