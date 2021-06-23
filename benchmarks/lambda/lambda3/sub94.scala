import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub94 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def loop(lam, l) = {
            lam match {
              case V(v) => {
                val _7 = {
                  def check_list(l, x) = {
                    l match {
                      case Nil() => { false }
                      case Cons(hd, tl) => {
                        if (hd == x) true else check_list(tl, x)
                      }
                    }
                  }
                  check_list(l, v)
                }
              }
              case P(v, e) => { loop(e, l ++ List(v)) }
              case C(e1, e2) => { loop(e1, l) && loop(e2, l) }
            }
          }
          loop(lam, Nil())
        }
    }
  )
    
  check(P("a", V("a")))
  check(P("a", P("a", V("a"))))
  check(P("a", P("b", C(V("a"), V("b")))))
  check(P("a", C(V("a"), P("b", V("a")))))
  check(P("x", C(P("y", V("y")), V("x"))))
  
  check(P("a", V("b")))
  check(P("a", C(V("a"), P("b", V("c")))))
  check(P("a", P("b", C(V("a"), V("c")))))
}