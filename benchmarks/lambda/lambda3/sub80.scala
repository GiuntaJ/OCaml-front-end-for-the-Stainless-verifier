import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub80 {
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
          def checklam(e, l) = {
            l match {
              case V(n) => {
                val _7 = {
                  def checkenv(e) = {
                    e match {
                      case Nil() => { false }
                      case Cons(hd, tl) => { if (n == hd) true else checkenv(tl)
                      }
                    }
                  }
                  checkenv(e)
                }
              }
              case P(v, l) => { checklam(v :: e, l) }
              case C(l1, l2) => { checklam(e, l1) && checklam(e, l2) }
            }
          }
          checklam(Nil(), lam)
        }
    }
  ) 
}