import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub167 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def checking: (Exp, List[Var]) => Boolean = {
            case (aexp, vlist) =>
              {
                aexp match {
                  case V(s) => {
                    vlist match {
                      case Nil() => { false }
                      case Cons(hd, tl) => {
                        if (s == hd) true else checking(V(s), tl)
                      }
                    }
                  }
                  case P(v, exp1) => { checking(exp1, v :: vlist) }
                  case C(exp1, exp2) => {
                    checking(exp1, vlist) && checking(exp2, vlist)
                  }
                }
            }
          }
          checking(exp, Nil())
        }
    }
  )
}