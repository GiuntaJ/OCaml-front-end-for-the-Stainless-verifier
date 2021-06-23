import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub57 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          def checkp(((va, ex))) = {
            ex match {
              case V(va2) => { if (va == va2) true else false }
              case P(va2, ex2) => {
                ex2 match {
                  case C(ex3, ex4) => {
                    
                      if (
                        (checkp(va, ex3) || checkp(va2, ex3)) &&
                        (checkp(va, ex4) || checkp(va2, ex4))
                      ) {
                        true 
                      } else {
                        false
                      }
                  }
                  case _ => {
                    if (checkp(va, ex2) || checkp(va2, ex2)) true else false
                  }
                }
              }
              case C(ex2, ex3) => {
                if (checkp(va, ex2) && checkp(va, ex3)) true else false
              }
            }
          }
          e match {
            case V(va) => { false }
            case P(va, ex) => { checkp(va, ex) }
            case C(ex, ex2) => { if (check(ex) && check(ex2)) true else false }
          }
        }
    }
  )
}