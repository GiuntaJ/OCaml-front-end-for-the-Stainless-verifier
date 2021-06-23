import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub115 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Lambda_env = List[Var]
  
  def ifinlist: (Var, Lambda_env) => Boolean = {
    case (x, l_e) =>
      {
        l_e match {
          case Nil() => { false }
          case Cons(hd, tl) => {
            hd match {
              case var_in_env => {
                if (x == var_in_env) true else ifinlist(x, tl)
              }
            }
          }
        }
    }
  }
  
  def check_dd: (Lambda, Lambda_env) => Boolean = {
    case (lam, l_e) =>
      {
        lam match {
          case V(x) => { ifinlist(x, l_e) }
          case P(x, l) => { check_dd(l, x :: l_e) }
          case C(l1, l2) => { check_dd(l1, l_e) && check_dd(l2, l_e) }
        }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { check_dd(lam, Nil()) } )
}
