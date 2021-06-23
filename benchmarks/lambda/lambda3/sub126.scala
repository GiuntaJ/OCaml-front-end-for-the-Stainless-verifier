import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub126 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val ex_env: List[A] = Nil()
  
  
  def find: (Var, List[Var]) => Boolean = {
    case (x, env) =>
      {
        env match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == x) true else find(x, tl) }
        }
    }
  }
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def check_: (Lambda, List[Var]) => Boolean = {
            case (lam, ev) =>
              {
                lam match {
                  case P(x, l) => { check_(l, ev ++ List(x)) }
                  case V(x) => { find(x, ev) }
                  case C(l1, l2) => { check_(l1, ev) && check_(l2, ev) }
                }
            }
          }
          check_(lam, ex_env)
        }
    }
  )
      
      
}
