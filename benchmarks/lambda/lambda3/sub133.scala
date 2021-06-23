import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub133 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  
  type T = List[Var]
  val empty: List[A] = Nil()
  def find[A](bvar: List[A], x: A): Boolean = {
    bvar match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == x) true else find(tl, x) }
    }
  }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def my_check(bvar, lamb) = {
            lamb match {
              case V(var0) => { find(bvar, var0) }
              case P(var0, lambda1) => { my_check(var0 :: bvar, lambda1) }
              case C(lambda1, lambda2) => {
                my_check(bvar, lambda1) && my_check(bvar, lambda2)
              }
            }
          }
          my_check(Nil(), lam)
        }
    }
  )
    
  check(P("a", P("b", C(V("a"), V("c")))))
}