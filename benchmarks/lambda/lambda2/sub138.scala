import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub138 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          val varlst = Nil()
          val _5 = {
            def checklst: (List[Var], Var) => Boolean = {
              case (lst, var0) =>
                {
                  lst match {
                    case Nil() => { false }
                    case Cons(n, lst2) => {
                      if (n == var0) true else checklst(lst2, var0)
                    }
                  }
              }
            }
            val _6 = {
              def checkvar: (Exp, List[Var]) => Boolean = {
                case (exp, lst) =>
                  {
                    exp match {
                      case V(v) => { checklst(lst, v) }
                      case P(v, e) => { checkvar(e, v :: lst) }
                      case C(e1, e2) => { checkvar(e1, lst) && checkvar(e2, lst)
                      }
                    }
                }
              }
              checkvar(exp, varlst)
            }
          }
        }
    }
  )	
  				
}