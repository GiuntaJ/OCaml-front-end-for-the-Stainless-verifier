import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub59 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def isinList[A](a: A, l: List[A]): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(h, t) => { if (h == a) true else isinList(a, t) }
    }
  }
  def validCheck: (Exp, List[Var]) => Boolean = {
    case (e, l) =>
      {
        e match {
          case V(_v) => { isinList(_v, l) }
          case P(_v, _e) => {
            val _2 = {
              val _l = _v :: l
              validCheck(_e, _l)
            }
          }
          case C(_e1, _e2) => { validCheck(_e1, l) && validCheck(_e2, l) }
        }
    }
  }
  val check: Exp => Boolean = ( (e) => { validCheck(e, Nil()) } )
}