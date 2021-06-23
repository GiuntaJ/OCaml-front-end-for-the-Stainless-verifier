import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub56 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def vcheck: (Var, List[Var]) => Boolean = {
            case (v, lst) =>
              {
                lst match {
                  case Nil() => { false }
                  case Cons(hd, tl) => { if (hd == v) true else vcheck(v, tl) }
                }
            }
          }
          val _5 = {
            def vlist: (Lambda, List[Var]) => Boolean = {
              case (lam, lst) =>
                {
                  lam match {
                    case V(v) => { vcheck(v, lst) }
                    case P(v, l) => { vlist(l, v :: lst) }
                    case C(l1, l2) => { vlist(l1, lst) && vlist(l2, lst) }
                  }
              }
            }
            vlist(lam, Nil())
          }
        }
    }
  )
}
