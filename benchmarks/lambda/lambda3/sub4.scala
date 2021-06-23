import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub4 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val empty: List[A] = Nil()
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def eval(l, env) = {
            l match {
              case V(x) => {
                env match {
                  case Nil() => { false }
                  case Cons(hd, tl) => { if (hd == x) true else eval(l, tl) }
                }
              }
              case P(x, l1) => { eval(l1, List(x) ++ env) }
              case C(l1, l2) => { eval(l1, env) && eval(l2, env) }
            }
          }
          eval(lam, empty)
        }
    }
  )
  
  
  
}
