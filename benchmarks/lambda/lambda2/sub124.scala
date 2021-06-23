import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub124 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def make_list: (Exp, (List[Var], List[Var])) => (List[Var], List[Var]) = {
            case (exp_0, (lst1, lst2)) =>
              {
                exp_0 match {
                  case V(v) => { (lst1, v :: lst2) }
                  case P(v, e) => { make_list(e, v :: lst1, lst2) }
                  case C(e1, e2) => { make_list(e1, make_list(e2, lst1, lst2)) }
                }
            }
          }
          val _5 = {
            def exist: (List[Var], Var) => Boolean = {
              case (lst, v) =>
                {
                  lst match {
                    case Nil() => { false }
                    case Cons(hd, tl) => { if (hd == v) true else exist(tl, v) }
                  }
              }
            }
            val _6 = {
              def real_check: (List[Var], List[Var]) => Boolean = {
                case (lst1, lst2) =>
                  {
                    lst2 match {
                      case Nil() => { true }
                      case Cons(hd, tl) => {
                        if (exist(lst1, hd)) real_check(lst1, tl) else false
                      }
                    }
                }
              }
              real_check(make_list(exp, Nil(), Nil()))
            }
          }
        }
    }
  )
}