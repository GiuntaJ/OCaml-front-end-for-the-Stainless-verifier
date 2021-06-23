import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub48 {
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
          def removevar: (Var, List[Var]) => List[Var] = {
            case (v, l) =>
              {
                l match {
                  case Nil() => { Nil() }
                  case Cons(h, t) => {
                    if (h == v) removevar(v, t) else List(h) ++ removevar(v, t)
                  }
                }
            }
          }
          val _5 = {
            def checklist: Lambda => List[Var] = (
              (lm) =>
                {
                  lm match {
                    case V(v) => { List(v) }
                    case P(v, l) => { removevar(v, checklist(l)) }
                    case C(l1, l2) => { checklist(l1) ++ checklist(l2) }
                  }
              }
            )
            checklist(lam) match {
              case Nil() => { true }
              case _ => { false }
            }
          }
        }
    }
  )
}