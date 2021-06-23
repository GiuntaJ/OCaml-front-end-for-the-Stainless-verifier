import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub120 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def check_val: (Exp, List[Var]) => Boolean = {
    case (exp, lst) =>
      {
        exp match {
          case V(x) => {
            lst match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (x == hd) true else check_val(V(x), tl)
              }
            }
          }
          case P(x, e1) => { check_val(e1, List(x) ++ lst) }
          case C(e1, e2) => {
            
              if (
                check_val(e1, lst) == true && check_val(e2, lst) == true
              ) {
                true 
              } else {
                false
              }
          }
        }
    }
  }
  
  val check: Exp => Boolean = ( (exp) => { check_val(exp, Nil()) } )
}