import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub129 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check_lst(lst, elem) = {
    lst match {
      case Cons(hd, tl) => { if (hd == elem) true else check_lst(tl, elem) }
      case Nil() => { false }
    }
  }
   
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def check_helper(exp, bound_list) = {
            exp match {
              case P(e1, e2) => {
                val _7 = {
                  val lis = e1 :: bound_list
                  check_helper(e2, lis)
                }
              }
              case V(v) => { check_lst(bound_list, v) }
              case C(e1, e2) => {
                check_helper(e1, bound_list) && check_helper(e2, bound_list)
              }
            }
          }
          val _8 = {
            val bound_list = Nil()
            check_helper(exp, bound_list)
          }
        }
    }
  )
}