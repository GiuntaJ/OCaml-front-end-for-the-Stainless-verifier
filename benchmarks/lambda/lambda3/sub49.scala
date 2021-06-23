import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub49 {
  /*2*/
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
   def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def sub: (Lambda, List[Var]) => Boolean = {
            case (lam, v1) =>
              {
                lam match {
                  case V(v) => { v1.exists(( (x) => { x == v } )) }
                  case P(v, lam) => { sub(lam, v :: v1) }
                  case C(lam1, lam2) => {
                    if (sub(lam1, v1)) sub(lam2, v1) else false
                  }
                }
            }
          }
          sub(lam, Nil())
        }
    }
  )
}