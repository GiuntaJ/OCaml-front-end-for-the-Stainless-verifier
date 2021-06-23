import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub113 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def eval: (Exp, List[Var]) => Boolean = {
    case (exp, lst) =>
      {
        exp match {
          case V(var0) => {
            lst match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (var0 == hd) true else eval(exp, tl) }
            }
          }
          case P(var0, exp2) => { eval(exp2, var0 :: lst) }
          case C(exp1, exp2) => { eval(exp1, lst) && eval(exp2, lst) }
        }
    }
  }
  
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          val state = Nil()
          exp match {
            case V(var0) => { eval(exp, state) }
            case P(var0, exp) => { eval(exp, var0 :: state) }
            case C(exp1, exp2) => { eval(exp1, state) && eval(exp2, state) }
          }
        }
    }
  )
}