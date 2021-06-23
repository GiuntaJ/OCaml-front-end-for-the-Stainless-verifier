import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub54 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def check_0(lam_0, vars) = {
            lam_0 match {
              case V(x) => { vars.exists(( (a) => { a == x } )) }
              case P(x, l) => { check_0(l, x :: vars) }
              case C(l1, l2) => { check_0(l1, vars) && check_0(l2, vars) }
            }
          }
          check_0(lam, Nil())
        }
    }
  )
}