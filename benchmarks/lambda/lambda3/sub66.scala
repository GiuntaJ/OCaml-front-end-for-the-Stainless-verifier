import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub66 {
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
          def find_var(var0, env) = {
            env match {
              case Nil() => { Nil() }
              case Cons(hd, tl) => {
                if (hd == var0) find_var(var0, tl) else hd :: find_var(var0, tl)
              }
            }
          }
          val _5 = {
            def eval(lam) = {
              lam match {
                case V(x) => { List(x) }
                case P(x, e) => { find_var(x, eval(e)) }
                case C(e1, e2) => { eval(e1) ++ eval(e2) }
              }
            }
            if (eval(lam) == Nil()) true else false
          }
        }
    }
  )
}