import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub67 {
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
          def checker: (Lambda, List[Var], Boolean) => Boolean = {
            case (lam1, lst, b) =>
              {
                lam1 match {
                  case V(x) => {
                    val _7 = {
                      def help: (Var, List[Var]) => Boolean = {
                        case (v, vlst) =>
                          {
                            vlst match {
                              case Nil() => { false }
                              case Cons(hd, tl) => {
                                if (v == hd) true else help(v, tl)
                              }
                            }
                        }
                      }
                      help(x, lst)
                    }
                  }
                  case P(v, l) => { checker(l, v :: lst, b) && b }
                  case C(l1, l2) => {
                    (checker(l1, lst, b) && checker(l2, lst, b)) && b
                  }
                }
            }
          }
          checker(lam, Nil(), true)
        }
    }
  )
}
