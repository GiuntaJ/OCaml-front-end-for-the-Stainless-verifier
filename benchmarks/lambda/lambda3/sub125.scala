import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub125 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lamb) =>
      {
        val _4 = {
          def find: (Var, List[Var]) => Boolean = {
            case (x, lst) =>
              {
                lst match {
                  case Nil() => { false }
                  case Cons(hd, tl) => { if (hd == x) true else find(x, tl) }
                }
            }
          }
          val _5 = {
            def check_sub: (Lambda, List[Var]) => Boolean = {
              case (lamb, lst) =>
                {
                  lamb match {
                    case V(v) => { find(v, lst) }
                    case P(v, lam) => { check_sub(lam, v :: lst) }
                    case C(lam1, lam2) => {
                      check_sub(lam1, lst) && check_sub(lam2, lst)
                    }
                  }
              }
            }
            check_sub(lamb, Nil())
          }
        }
    }
  )
}