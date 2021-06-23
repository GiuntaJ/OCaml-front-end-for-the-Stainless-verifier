import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub117 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def checklst: (Exp, List[Var]) => Boolean = {
            case (ee, llst) =>
              {
                ee match {
                  case V(vf) => {
                    llst match {
                      case Nil() => { false }
                      case Cons(hd, tl) => {
                        if (hd == vf) true else checklst(ee, tl)
                      }
                    }
                  }
                  case P(vf, expf) => { checklst(expf, vf :: llst) }
                  case C(expf1, expf2) => {
                    checklst(expf1, llst) && checklst(expf2, llst)
                  }
                }
            }
          }
          val _5 = {
            val varlist = Nil()
            exp match {
              case P(v1, exp1) => { checklst(exp1, v1 :: varlist) }
              case _ => { checklst(exp, varlist) }
            }
          }
        }
    }
  )
}