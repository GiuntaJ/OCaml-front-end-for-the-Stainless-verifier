import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub101 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def para: (Exp, List[String]) => Boolean = {
    case (exp, lst) =>
      {
        exp match {
          case V(x) => {
            val _2 = {
              def ac: (List[String], String) => Boolean = {
                case (oh, ho) =>
                  {
                    oh match {
                      case Nil() => { false }
                      case Cons(hd, tl) => { if (hd == ho) true else ac(tl, ho)
                      }
                    }
                }
              }
              ac(lst, x)
            }
          }
          case P(x, e) => { para(e, x :: lst) }
          case C(e1, e2) => { para(e1, lst) && para(e2, lst) }
        }
    }
  }
  
  
    def check: Exp => Boolean = ( (exp) => { para(exp, Nil()) } )
}