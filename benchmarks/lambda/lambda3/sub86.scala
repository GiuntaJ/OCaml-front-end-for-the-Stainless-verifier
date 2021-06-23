import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub86 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def check_in(exp, env) = {
            exp match {
              case V(x) => { env(x) }
              case P(x, e) => {
                check_in(e, ( (y) => { if (y == x) true else env(y) } ))
              }
              case C(l, r) => { check_in(l, env) && check_in(r, env) }
            }
          }
          check_in(lam, ( (y) => { false } ))
        }
    }
  )
  
  
  val pgm1: Lambda = P("a", V("a"))
  val pgm2: Lambda = P("a", P("a", V("a")))
  val pgm3: Lambda = P("a", P("b", C(V("a"), V("b"))))
  val pgm4: Lambda = P("a", C(V("a"), P("b", V("a"))))
  val pgm5: Lambda = P("a", V("b"))
  val pgm6: Lambda = P("a", C(V("a"), P("b", V("c"))))
  val pgm7: Lambda = P("a", P("b", C(V("a"), V("c"))))
  check(pgm1)
  check(pgm2)
  check(pgm3)
  check(pgm4)
  check(pgm5)
  check(pgm6)
  check(pgm7)
}