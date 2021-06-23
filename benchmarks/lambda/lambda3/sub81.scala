import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub81 {
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
          def che(lam, l) = {
            lam match {
              case V(x) => {
                val _7 = {
                  def search(lst) = {
                    lst match {
                      case Nil() => { false }
                      case Cons(hd, tl) => { if (hd == x) true else search(tl) }
                    }
                  }
                  search(l)
                }
              }
              case P(x, ld) => { che(ld, x :: l) }
              case C(ld1, ld2) => { che(ld1, l) && che(ld2, l) }
            }
          }
          che(lam, Nil())
        }
    }
  )
}