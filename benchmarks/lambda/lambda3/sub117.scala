import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub117 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def is_in_list[A](a: A, lst: List[A]): Boolean = {
    lst match {
      case Nil() => { false }
      case Cons(x, xs) => { if (x == a) true else is_in_list(a, xs) }
    }
  }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def checkocc(e, varlist) = {
            e match {
              case V(var0) => { if (is_in_list(var0, varlist)) true else false }
              case P(var0, lamb) => { checkocc(lamb, var0 :: varlist) }
              case C(lamb1, lamb2) => {
                checkocc(lamb1, varlist) && checkocc(lamb2, varlist)
              }
            }
          }
          checkocc(lam, Nil())
        }
    }
  )
  
  check(P("a", C(V("a"), P("b", V("a")))))
  check(P("a", P("b", C(V("a"), V("c")))))
  check(P("a", C(V("a"), P("b", V("c")))))
}