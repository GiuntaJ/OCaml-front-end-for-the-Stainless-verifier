import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub62 {
  /*	Problem 2	*/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def reccheck: (Lambda, List[Var]) => Boolean = {
            case (l, vlist) =>
              {
                val _7 = {
                  def listfind: (Var, List[Var]) => Boolean = {
                    case (x, lst) =>
                      {
                        lst match {
                          case Nil() => { false }
                          case Cons(hd, tl) => {
                            if (hd == x) true else listfind(x, tl)
                          }
                        }
                    }
                  }
                  l match {
                    case V(v) => { listfind(v, vlist) }
                    case P(v, l_0) => { reccheck(l_0, v :: vlist) }
                    case C(l1, l2) => {
                      reccheck(l1, vlist) && reccheck(l2, vlist)
                    }
                  }
                }
            }
          }
          reccheck(lam, Nil())
        }
    }
  )
}
