import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub160 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
    def checklst(lst, val1) = {
    lst match {
      case Cons(hd, tl) => { if (hd == val1) false else checklst(tl, val1) }
      case Nil() => { true }
    }
  }
  
    def checkExp(lst, exp) = {
    exp match {
      case V(var0) => { if (checklst(lst, var0)) false else true }
      case P(var0, exp) => {
        
          if (
            checklst(lst, var0)
          ) {
            checkExp(var0 :: lst, exp) 
          } else {
            checkExp(lst, exp)
          }
      }
      case C(exp1, exp2) => {
        
          if (
            checkExp(lst, exp1) eq true && checkExp(lst, exp2) eq true
          ) {
            true 
          } else {
            false
          }
      }
    }
  }
  
     val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          val varlst = Nil()
          checkExp(varlst, exp)
        }
    }
  )
}