import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub135 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def scan: (List[String], String) => Boolean = {
    case (lst, s) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(h, t) => { if (h == s) true else scan(t, s) }
        }
    }
  }
  
    def rcheck: (Exp, List[String]) => Boolean = {
    case (exp, stack) =>
      {
        exp match {
          case V(a) => { scan(stack, a) }
          case P(a, b) => { rcheck(b, a :: stack) }
          case C(a, b) => { rcheck(a, stack) && rcheck(b, stack) }
        }
    }
  }
  
  val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          val stack = Nil()
          rcheck(exp, stack)
        }
    }
  ) 
}