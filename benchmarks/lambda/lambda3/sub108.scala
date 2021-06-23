import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub108 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def allbound: (List[Var], Lambda) => Boolean = {
            case (varlst, expr) =>
              {
                expr match {
                  case V(v) => {
                    
                      if (
                        varlst.filter(( (x) => { if (x == v) true else false } )) ==
                          Nil()
                      ) {
                        false 
                      } else {
                        true
                      }
                  }
                  case P(x, nexpr) => { allbound(x :: varlst, nexpr) }
                  case C(expr1, expr2) => {
                    allbound(varlst, expr1) && allbound(varlst, expr2)
                  }
                }
            }
          }
          allbound(Nil(), lam)
        }
    }
  )
}