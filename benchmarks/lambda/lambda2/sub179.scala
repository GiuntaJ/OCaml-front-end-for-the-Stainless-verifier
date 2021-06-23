import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub179 {
  
    	sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def compare: (Exp, List[String]) => Boolean = {
    case (exp, lst) =>
      {
        exp match {
          case V(x) => {
            lst match {
              case Cons(hd, tl) => { if (x == hd) true else compare(exp, tl) }
              case Nil() => { false }
            }
          }
          case P(x, e1) => { compare(e1, x :: lst) }
          case C(e1, e2) => { compare(e1, lst) && compare(e2, lst) }
        }
    }
  }
  
  def check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          val lst = Nil()
          exp match {
            case V(x) => { false }
            case P(x, e1) => {
              val _7 = {
                val l = x :: lst
                compare(e1, l)
              }
            }
            case C(e1, e2) => { compare(e1, lst) && compare(e2, lst) }
          }
        }
    }
  )
}