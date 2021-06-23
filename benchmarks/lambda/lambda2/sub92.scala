import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub92 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def lcheck: (Exp, List[Var]) => Boolean = {
    case (exp, vlist) =>
      {
        exp match {
          case V(var1) => {
            vlist match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == var1) true else lcheck(exp, tl) }
            }
          }
          case P(var1, exp1) => { lcheck(exp1, var1 :: vlist) }
          case C(exp1, exp2) => { lcheck(exp1, vlist) && lcheck(exp2, vlist) }
        }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { lcheck(exp, Nil()) } )
}