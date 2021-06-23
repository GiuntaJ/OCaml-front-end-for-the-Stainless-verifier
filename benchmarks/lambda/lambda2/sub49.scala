import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub49 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
   def ch: (Exp, Var) => Boolean = {
    case (e, a) =>
      {
        e match {
          case V(b) => { if (a == b) true else false }
          case P(b, e) => {
            e match {
              case V(c) => { if (c == b || c == a) true else false }
              case P(c, e) => { ch(e, c) || ch(e, b) || ch(e, a) }
              case C(e1, e2) => {
                
                  if (
                    a == b
                  ) {
                    ch(e1, a) && ch(e2, a) 
                  } else {
                    (ch(e1, a) || ch(e1, b)) && (ch(e2, a) || ch(e2, b))
                  }
              }
            }
          }
          case C(e1, e2) => { ch(e1, a) && ch(e2, a) }
        }
    }
  }
  
    val check: Exp => Boolean = (
    (e) =>
      {
        e match {
          case V(a) => { false }
          case P(a, e) => { ch(e, a) }
          case C(e1, e2) => { false }
        }
    }
  )
  	
}