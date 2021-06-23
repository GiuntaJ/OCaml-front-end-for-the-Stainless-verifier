import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub104 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def checkvar(exp, mylist) = {
            exp match {
              case V(var0) => { if (mylist.contains(var0)) true else false }
              case P(var0, myexp) => { checkvar(myexp, mylist ++ List(var0)) }
              case C(exp1, exp2) => {
                checkvar(exp1, mylist) && checkvar(exp2, mylist)
              }
            }
          }
          checkvar(exp, Nil())
        }
    }
  ) 
}