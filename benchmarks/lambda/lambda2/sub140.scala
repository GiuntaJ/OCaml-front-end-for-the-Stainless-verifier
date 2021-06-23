import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub140 {
  /*
    1. You can modify the given function specifications as recursive.
    2. Do not modify the function names or types.
    3. It is free to define any helper functions.
  */
  
  sealed case class NotImplemented() extends Exception {}
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def varOfExp: Exp => Var = (
    (exp) =>
      {
        exp match {
          case V(var0) => { var0 }
          case P(var0, e) => { varOfExp(e) }
          case C(e1, e2) => { varOfExp(e2) }
        }
    }
  )
  
  
     val check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case P(var1, P(var2, e)) => {
            if (varOfExp(e) == var2 || varOfExp(e) == var1) true else false
          }
          case P(var1, C(e1, e2)) => { if (varOfExp(e2) == var1) true else false
          }
          case P(var1, V(var2)) => { if (var1 == var2) true else false }
          case _ => { assert(false, "NotImplemented") }
        }
    }
  )
}