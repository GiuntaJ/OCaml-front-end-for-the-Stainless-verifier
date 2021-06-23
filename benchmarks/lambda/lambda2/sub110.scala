import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub110 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def deletehd: (String, List[String]) => Boolean = {
            case (var0, l) =>
              {
                l match {
                  case Nil() => { false }
                  case Cons(hd, tl) => {
                    if (hd == var0) true else deletehd(var0, tl)
                  }
                }
            }
          }
          val _5 = {
            def isbound: (Exp, List[String]) => Boolean = {
              case (exp, lst) =>
                {
                  exp match {
                    case P(var0, exp1) => { isbound(exp1, var0 :: lst) }
                    case C(exp1, exp2) => {
                      isbound(exp1, lst) && isbound(exp2, lst)
                    }
                    case V(var0) => { deletehd(var0, lst) }
                  }
              }
            }
            isbound(exp, Nil())
          }
        }
    }
  )
}