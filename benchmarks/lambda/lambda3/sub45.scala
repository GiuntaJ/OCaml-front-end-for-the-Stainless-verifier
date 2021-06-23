import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub45 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def fchk: (Var, List[Var]) => List[Var] = {
            case (v, l) =>
              {
                l match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => {
                    if (v == hd) fchk(v, tl) else hd :: fchk(v, tl)
                  }
                }
            }
          }
          val _5 = {
            def eval: Lambda => List[Var] = (
              (lam) =>
                {
                  lam match {
                    case V(x) => { List(x) }
                    case P(x, e) => { fchk(x, eval(e)) }
                    case C(e1, e2) => { eval(e1) ++ eval(e2) }
                  }
              }
            )
            eval(lam) match {
              case Nil() => { true }
              case _ => { false }
            }
          }
        }
    }
  )
}