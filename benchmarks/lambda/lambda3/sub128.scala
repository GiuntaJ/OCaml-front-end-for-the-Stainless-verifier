import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub128 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def find: (Var, List[Var]) => Boolean = {
    case (x, lst) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == x) true else find(x, tl) }
        }
    }
  }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def checkrec: (Lambda, List[Var]) => Boolean = {
            case (lam, lst) =>
              {
                lam match {
                  case V(v) => { find(v, lst) }
                  case P(v, l) => { checkrec(l, v :: lst) }
                  case C(l1, l2) => { checkrec(l1, lst) && checkrec(l2, lst) }
                }
            }
          }
          checkrec(lam, Nil())
        }
    }
  )
}