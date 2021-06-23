import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub168 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def evaluate_vpc(expression, vpc) = {
    expression match {
      case V(a) => {
        val _2 = {
          def check(lst, st) = {
            lst match {
              case Nil() => { false }
              case Cons(head, tail) => {
                if (head == st) true else check(tail, st)
              }
            }
          }
          if (check(vpc, a)) true else false
        }
      }
      case P(pa, ex1) => {
        if (evaluate_vpc(ex1, vpc ++ List(pa))) true else false
      }
      case C(ex1, ex2) => {
        if (evaluate_vpc(ex1, vpc) && evaluate_vpc(ex2, vpc)) true else false
      }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { evaluate_vpc(exp, Nil()) } ) /* raise NotImplemented */ /* TODO */
}
