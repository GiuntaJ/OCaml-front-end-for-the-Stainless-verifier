import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub88 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  
  val pgm1: Lambda = P("a", V("a"))
  
  val pgm2: Lambda = P("a", V("b"))
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        lam match {
          case P(v1, e1) => {
            e1 match {
              case P(v2, e2) => { if (v1 == v2) true else false }
              case V(v2) => { if (v1 == v2) true else false }
              case C(V(v2), V(v3)) => {
                if (v1 == v2 || v1 == v3) true else false
              }
            }
          }
        }
    }
  )
        
  check(pgm1)
  check(pgm2)
}