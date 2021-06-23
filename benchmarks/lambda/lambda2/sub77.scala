import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub77 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case P(s, V(a)) => { if (s == a) true else false }
          case P(s, P(s1, e1)) => {
            e1 match {
              case C(e2, e3) => {
                (check(P(s, e2)) || check(P(s1, e2))) &&
                (check(P(s, e3)) || check(P(s1, e3)))
              }
              case _ => {
                if (check(P(s1, e1)) == true || check(P(s, e1))) true else false
              }
            }
          }
          case P(s, C(e1, e2)) => {
            (e1, e2) match {
              case (P(s1, e3), e) => {
                
                  if (
                    check(P(s, P(s1, e3))) && check(P(s, P(s1, e)))
                  ) {
                    true 
                  } else {
                    false
                  }
              }
              case (e, P(s1, e3)) => {
                
                  if (
                    check(P(s, P(s1, e3))) && check(P(s, P(s1, e)))
                  ) {
                    true 
                  } else {
                    false
                  }
              }
              case _ => {
                if (check(P(s, e1)) && check(P(s, e2))) true else false
              }
            }
          }
          case _ => { false }
        }
    }
  ) 
}