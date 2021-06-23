import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub102 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def lookup[A](ls: List[A], v: A): Boolean = {
    ls match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == v) true else lookup(tl, v) }
    }
  }
      
  def extend[A](ls: List[A], v: A): List[A] = { ls ++ List(v) }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def ch(vlist, lamb) = {
            lamb match {
              case V(x) => { lookup(vlist, x) }
              case P(nv, nb) => { ch(extend(vlist, nv), nb) }
              case C(a, b) => { ch(vlist, a) && ch(vlist, b) }
              case _ => { assert(false, "Failure with Undefined semantics") }
            }
          }
          ch(Nil(), lam)
        }
    }
  )
    
  
  
  
  
  
  
  val p1: Lambda = P("a", V("a"))
  val p2: Lambda = P("a", P("a", V("a")))
  val p3: Lambda = P("a", P("b", C(V("a"), V("b"))))
  val p4: Lambda = P("a", C(V("a"), P("b", V("a"))))
  
  val p5: Lambda = P("a", V("b"))
  val p6: Lambda = P("a", C(V("a"), P("b", V("c"))))
  val p7: Lambda = P("a", P("b", C(V("a"), V("c"))))
  
  check(p1)
  check(p2)
  check(p3)
  check(p4)
  check(p5)
  check(p6)
  check(p7)
}