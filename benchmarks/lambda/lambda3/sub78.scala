import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub78 {
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
          def checkenv(lama, env) = {
            lama match {
              case V(a) => {
                val _7 = {
                  def search(listl) = {
                    listl match {
                      case Cons(hd, tl) => { if (hd == a) true else search(tl) }
                      case _ => { false }
                    }
                  }
                  if (search(env)) true else false
                }
              }
              case P(a, b) => { checkenv(b, a :: env) }
              case C(a, b) => { checkenv(a, env) && checkenv(b, env) }
            }
          }
          checkenv(lam, Nil())
        }
    }
  )
}