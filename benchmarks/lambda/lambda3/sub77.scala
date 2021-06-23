import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub77 {
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
          def help(lam, env) = {
            lam match {
              case V(a) => { env.contains(a) }
              case P(a, b) => { help(b, a :: env) }
              case C(a, b) => { help(a, env) && help(b, env) }
            }
          }
          help(lam, Nil())
        }
    }
  )
}