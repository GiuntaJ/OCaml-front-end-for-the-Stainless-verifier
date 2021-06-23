import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub93 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def lookup_env: (List[Var], Var) => Boolean = {
            case (env, v) =>
              {
                env match {
                  case Nil() => { false }
                  case Cons(hd, tl) => { hd == v || lookup_env(tl, v) }
                }
            }
          }
          val _5 = {
            def check_0: (Lambda, List[Var]) => Boolean = {
              case (lam, env) =>
                {
                  lam match {
                    case V(v) => { lookup_env(env, v) }
                    case P(v, l) => { check_0(l, v :: env) }
                    case C(l1, l2) => { check_0(l1, env) && check_0(l2, env) }
                  }
              }
            }
            check_0(lam, Nil())
          }
        }
    }
  )
  
  
  
  /******************************************************************************************************/
  val l1: Lambda = P("a", V("a"))
  val l2: Lambda = P("a", P("a", V("a")))
  val l3: Lambda = P("a", P("b", C(V("a"), V("b"))))
  val l4: Lambda = P("a", C(V("a"), P("b", V("a"))))
  val i1: Lambda = P("a", V("b"))
  val i2: Lambda = P("a", C(V("a"), P("b", V("c"))))
  val i3: Lambda = P("a", P("b", C(V("a"), V("c"))))
  val test1: Lambda = P("x", V("x"))
  val test2: Lambda = C(P("x", V("x")), V("y"))
  val test3: Lambda = P("x", C(V("x"), V("y")))
  val tets4: Lambda = C(P("x", C(V("x"), V("y"))), V("z"))
  val test5: Lambda = C(P("x", P("y", V("x"))), V("z"))
    
  check(test1)
}