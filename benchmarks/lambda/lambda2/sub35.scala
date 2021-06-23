import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub35 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
  def seek: (Var, List[Var]) => Boolean = {
    case (v, li) =>
      {
        li match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == v) true else seek(v, tl) }
        }
    }
  }
  
  val check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          def check_sub: (Exp, List[Var]) => Boolean = {
            case (e, li) =>
              {
                e match {
                  case V(v) => { seek(v, li) }
                  case P(v, e) => { check_sub(e, v :: li) }
                  case C(e1, e2) => { check_sub(e1, li) && check_sub(e2, li) }
                }
            }
          }
          check_sub(e, Nil())
        }
    }
  )
}
