import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub92 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def find: (List[Var], Var) => Boolean = {
    case (l, v) =>
      {
        l match {
          case Nil() => { false }
          case Cons(h, t) => { if (h == v) true else find(t, v) }
        }
    }
  }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          val s = Nil()
          val _5 = {
            def checking: (Lambda, List[Var]) => Boolean = {
              case (lamb, stack) =>
                {
                  lamb match {
                    case V(a) => { find(stack, a) }
                    case P(v, l) => { checking(l, v :: stack) }
                    case C(l1, l2) => {
                      checking(l1, stack) && checking(l2, stack)
                    }
                  }
              }
            }
            checking(lam, s)
          }
        }
    }
  )
    
  val good1: Lambda = P("a", V("a"))
  val good2: Lambda = P("a", P("a", V("a")))
  val good3: Lambda = P("a", P("b", C(V("a"), V("b"))))
  val good4: Lambda = P("a", C(V("a"), P("b", V("a"))))
  
  val ill1: Lambda = P("a", V("b"))
  val ill2: Lambda = P("a", C(V("a"), P("b", V("c"))))
  val ill3: Lambda = P("a", P("b", C(V("a"), V("c"))))
  
  check(good1)
  check(good2)
  check(good3)
  check(good4)
  check(ill1)
  check(ill2)
  check(ill3)
}
