import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub107 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def e(exp) = {
            exp match {
              case V(v) => { false }
              case P(p, l) => {
                val _7 = {
                  def f(p, l) = {
                    l match {
                      case V(a) => { if (a == p) true else false }
                      case P(b, c) => { if (f(p, c) || f(b, c)) true else false
                      }
                      case C(d, e) => { if (f(p, e) && f(p, d)) true else false
                      }
                    }
                  }
                  f(p, l)
                }
              }
              case C(n, m) => {
                (n, m) match {
                  case (V(x), V(y)) => { false }
                  case (P(x, y), P(z, t)) => {
                    
                      if (
                        (e(P(x, y)) || e(P(z, y))) && (e(P(x, t)) || e(P(z, t)))
                      ) {
                        true 
                      } else {
                        false
                      }
                  }
                  case (V(x), P(y, z)) => {
                    
                      if (
                        (e(P(y, n)) || e(C(n, z))) && (e(P(y, z)) || e(C(n, z)))
                      ) {
                        true 
                      } else {
                        false
                      }
                  }
                  case (P(y, z), V(x)) => {
                    
                      if (
                        (e(P(y, m)) || e(C(m, z))) && (e(P(y, z)) || e(C(m, z)))
                      ) {
                        true 
                      } else {
                        false
                      }
                  }
                  case (V(x), C(y, z)) => {
                    
                      if (
                        (e(C(n, y)) || e(C(n, z))) &&
                        (e(C(y, z)) || e(C(y, n))) && (e(C(z, n)) || e(C(z, y)))
                      ) {
                        true 
                      } else {
                        false
                      }
                  }
                  case (C(y, z), V(x)) => {
                    
                      if (
                        (e(C(m, y)) || e(C(m, z))) &&
                        (e(C(y, z)) || e(C(y, m))) && (e(C(z, m)) || e(C(z, y)))
                      ) {
                        true 
                      } else {
                        false
                      }
                  }
                  case (P(x, y), C(z, t)) => {
                    
                      if (
                        (e(P(x, y)) || e(C(y, z)) || e(C(y, t))) &&
                        (e(P(x, z)) || e(C(z, t)) || e(C(z, y))) &&
                        (e(P(x, t)) || e(C(t, y)) || e(C(t, z)))
                      ) {
                        true 
                      } else {
                        false
                      }
                  }
                  case (C(z, t), P(x, y)) => {
                    
                      if (
                        (e(P(x, y)) || e(C(y, z)) || e(C(y, t))) &&
                        (e(P(x, z)) || e(C(z, t)) || e(C(z, y))) &&
                        (e(P(x, t)) || e(C(t, y)) || e(C(t, z)))
                      ) {
                        true 
                      } else {
                        false
                      }
                  }
                  case (C(x, y), C(z, t)) => {
                    
                      if (
                        (e(C(x, y)) || e(C(x, z)) || e(C(x, t))) &&
                        (e(C(y, z)) || e(C(y, t)) || e(C(y, x))) &&
                        (e(C(z, t)) || e(C(z, x)) || e(C(z, y))) &&
                        (e(C(t, x)) || e(C(t, y)) || e(C(t, z)))
                      ) {
                        true 
                      } else {
                        false
                      }
                  }
                }
              }
            }
          }
          e(exp)
        }
    }
  )
}