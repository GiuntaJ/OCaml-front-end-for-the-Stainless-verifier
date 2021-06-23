import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub132 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def expToVar: Exp => Var = (
    (exp) =>
      {
        exp match {
          case V(var0) => { var0 }
          case P(var0, ex) => { expToVar(ex) }
          case C(ex1, ex2) => { expToVar(ex2) }
        }
    }
  )
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def pcheck: (Exp, List[Var]) => Boolean = {
            case (aexp, vlist) =>
              {
                aexp match {
                  case V(s) => {
                    vlist match {
                      case Nil() => { false }
                      case Cons(h, t) => { if (s == h) true else pcheck(V(s), t)
                      }
                    }
                  }
                  case P(v, exp) => { pcheck(exp, v :: vlist) }
                  case C(exp1, exp2) => {
                    pcheck(exp1, vlist) && pcheck(exp2, vlist)
                  }
                }
            }
          }
          pcheck(exp, Nil())
        }
    }
  )
}