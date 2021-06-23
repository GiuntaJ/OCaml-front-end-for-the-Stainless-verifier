import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub50 {
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
          def pop_value: (List[Var], Var) => List[Var] = {
            case (l, v) =>
              {
                l match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => {
                    if (hd == v) pop_value(tl, v) else hd :: pop_value(tl, v)
                  }
                }
            }
          }
          val _5 = {
            def free_value: Lambda => List[Var] = (
              (l) =>
                {
                  l match {
                    case V(v) => { List(v) }
                    case P(v0, l0) => { pop_value(free_value(l0), v0) }
                    case C(l1, l2) => { free_value(l1) ++ free_value(l2) }
                  }
              }
            )
            free_value(lam) eq Nil()
          }
        }
    }
  )
}
