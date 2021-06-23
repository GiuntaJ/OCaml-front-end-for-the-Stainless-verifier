import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub157 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def lcheck: (Exp, List[String]) => Boolean = {
    case (exp, lst) =>
      {
        exp match {
          case V(a) => {
            lst match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == a) true else lcheck(V(a), tl) }
            }
          }
          case P(a, b) => { lcheck(b, a :: lst) }
          case C(a, b) => { lcheck(a, lst) && lcheck(b, lst) }
        }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { lcheck(exp, Nil()) } )
}