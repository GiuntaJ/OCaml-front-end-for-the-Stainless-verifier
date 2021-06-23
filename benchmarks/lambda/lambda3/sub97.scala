import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub97 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def list_search: (List[Var], Var) => Boolean = {
            case (lst, v) =>
              {
                lst match {
                  case Nil() => { false }
                  case Cons(hd, tl) => {
                    if (hd == v) true else list_search(tl, v)
                  }
                }
            }
          }
          val _5 = {
            def trace: (Lambda, List[Var]) => Boolean = {
              case (la, var_lst) =>
                {
                  la match {
                    case V(v) => { if (list_search(var_lst, v)) true else false
                    }
                    case P(v, nla) => {
                      val _8 = {
                        val new_lst = v :: var_lst
                        trace(nla, new_lst)
                      }
                    }
                    case C(nla1, nla2) => {
                      trace(nla1, var_lst) && trace(nla2, var_lst)
                    }
                  }
              }
            }
            trace(lam, Nil())
          }
        }
    }
  )
    
}