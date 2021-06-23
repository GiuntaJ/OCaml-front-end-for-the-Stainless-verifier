import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub36 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
  	def change: (Var, Exp) => Exp = {
    case (v, e) =>
      {
        e match {
          case V(v_0) => { V(v_0) }
          case P(v_0, e_0) => { change(v_0, e_0) }
          case C(e1, e2) => {
            
              if (
                V(v) == e1
              ) {
                if (V(v) == e2) change(v, e1) else change(v, e2) 
              } else if (
                V(v) == e2
              ) {
                change(v, e1) 
              } else {
                change(v, V(v))
              }
          }
        }
    }
  }
  
    val check: Exp => Boolean = (
    (e) =>
      {
        e match {
          case V(v) => { false }
          case P(v, e_0) => { if (V(v) == change(v, e_0)) true else false }
          case C(e1, e2) => { false }
        }
    }
  )
}