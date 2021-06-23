import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub129 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def findlst: (List[Var], Var) => Boolean = {
    case (lst, v) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == v) true else findlst(tl, v) }
        }
    }
  }
  
  def mycheck: (Lambda, List[Var]) => Boolean = {
    case (lam, lst) =>
      {
        lam match {
          case V(x) => { findlst(lst, x) }
          case P(x, e) => { mycheck(e, x :: lst) }
          case C(e1, e2) => { mycheck(e1, lst) && mycheck(e2, lst) }
        }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { mycheck(lam, Nil()) } )
  
  val t1: Lambda = P("a", V("a"))
  val t2: Lambda = P("a", P("a", V("a")))
  val t3: Lambda = P("a", P("b", C(V("a"), V("b"))))
  val t4: Lambda = P("a", C(V("a"), P("b", V("a"))))
  
  val t5: Lambda = P("a", V("b"))
  val t6: Lambda = P("a", C(V("a"), P("b", V("c"))))
  val t7: Lambda = P("a", P("b", C(V("a"), V("c"))))
  
  check(t1)
  check(t2)
  check(t3)
  check(t4)
  
  check(t5)
  check(t6)
  check(t7)
}