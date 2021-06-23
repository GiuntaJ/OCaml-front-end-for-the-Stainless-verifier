import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub91 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        lam match {
          case V(v1) => { true }
          case P(v2, lam1) => {
            lam1 match {
              case V(v2) => { true }
              case P(v3, lam2) => { check(lam2) }
              case C(lam3, lam4) => {
                val _6 = {
                  val bool1 = check(lam3)
                  val _7 = {
                    val bool2 = check(lam4)
                    if ((bool1 == bool2) == true) true else false
                  }
                }
              }
              case C(lam1, lam2) => {
                val _2 = {
                  val bool3 = check(lam1)
                  val _3 = {
                    val bool4 = check(lam2)
                    if ((bool3 == bool4) == true) true else false
                  }
                }
              }
              case _ => { false }
            }
          }
        }
    }
  )
}