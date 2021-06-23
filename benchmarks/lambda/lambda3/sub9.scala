import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub9 {
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
          def eval: (Lambda, List[Var]) => List[Var] = {
            case (lam, ll) =>
              {
                lam match {
                  case V(v) => {
                    ll match {
                      case Nil() => { "false" :: ll }
                      case Cons(hd, tl) => {
                        if (hd == v) ll else hd :: eval(V(v), tl)
                      }
                    }
                  }
                  case P(v, l) => {
                    val _7 = {
                      val ll2 = v :: ll
                      eval(l, ll2)
                    }
                  }
                  case C(l1, l2) => { eval(l2, eval(l1, ll)) }
                }
            }
          }
          val _8 = {
            def compare: List[Var] => Boolean = (
              (ll) =>
                {
                  ll match {
                    case Nil() => { true }
                    case Cons(hd, tl) => {
                      if (hd == "false") false else compare(tl)
                    }
                  }
              }
            )
            compare(eval(lam, Nil()))
          }
        }
    }
  )
}
