import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub100 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(var0) => { false }
          case P(var1, V(var2)) => { if (var1 == var2) true else false }
          case P(var1, C(V(var2), P(var3, V(var4)))) => {
            if (var1 == var4 || var2 == var4 || var3 == var4) true else false
          }
          case P(var1, P(var2, V(var3))) => {
            if (var1 == var3 || var2 == var3) true else false
          }
          case P(var1, P(var2, C(V(var3), V(var4)))) => {
            
              if (
                (var1 == var3 || var2 == var3) && (var1 == var4 || var2 == var4)
              ) {
                true 
              } else {
                false
              }
          }
          case C(V(var1), V(var2)) => { false }
          case P(var1, P(var2, P(var3, V(var4)))) => {
            if (var1 == var4 || var2 == var4 || var3 == var4) true else false
          }
        }
    }
  )
}