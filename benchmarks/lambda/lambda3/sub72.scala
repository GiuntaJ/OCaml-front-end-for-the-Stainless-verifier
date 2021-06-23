import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub72 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def check2: (Lambda, List[Var], Boolean) => Boolean = {
            case (lamb, lst, b) =>
              {
                lamb match {
                  case V(x) => {
                    val _7 = {
                      def helpfunc2: (Var, List[Var]) => Boolean = {
                        case (v, vlst) =>
                          {
                            vlst match {
                              case Nil() => { false }
                              case Cons(hd, tl) => {
                                if (v == hd) true else helpfunc2(v, tl)
                              }
                            }
                        }
                      }
                      helpfunc2(x, lst)
                    }
                  }
                  case P(v, l) => { check2(l, v :: lst, b) && b }
                  case C(l1, l2) => {
                    (check2(l1, lst, b) && check2(l2, lst, b)) && b
                  }
                }
            }
          }
          check2(lam, Nil(), true)
        }
    }
  )
}