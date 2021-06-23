import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub85 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
     def strbool: (String, List[String]) => Boolean = {
    case (var0, strlist) =>
      {
        strlist match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == var0) true else strbool(var0, tl) }
        }
    }
  }
  
     def expbool: (Exp, List[String]) => Boolean = {
    case (exp, strlist) =>
      {
        exp match {
          case V(var0) => { strbool(var0, strlist) }
          case P(var0, exp) => { expbool(exp, var0 :: strlist) }
          case C(e1, e2) => {
            
              if (
                expbool(e1, strlist) == true && expbool(e2, strlist) == true
              ) {
                true 
              } else {
                false
              }
          }
        }
    }
  }
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(var0) => { false }
          case P(var0, exp) => { expbool(exp, List(var0)) }
          case C(e1, e2) => {
            
              if (
                (expbool(e1, Nil()) && expbool(e2, Nil())) == true
              ) {
                true 
              } else {
                false
              }
          }
        }
    }
  )
     /*| C(e1, e2) -> if ((expbool(e1, []) && (expbool(e2, [])) = true then true else false
   | C(e1, e2) -> if (expbool(e1, []) = true) && (expbool(e2, []) = true) then true else false*/
}