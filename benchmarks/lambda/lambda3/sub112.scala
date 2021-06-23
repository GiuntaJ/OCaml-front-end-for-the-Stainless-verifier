import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub112 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def extend[A](x: A, l: List[A]): List[A] = { List(x) ++ l }
  def lookup[A](x: A, l: List[A]): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (x == hd) true else lookup(x, tl) }
    }
  }
  
  def check_help(lam, lst) = {
    lam match {
      case V(x) => { lookup(x, lst) }
      case P(x, la) => { check_help(la, extend(x, lst)) }
      case C(la1, la2) => { check_help(la1, lst) && check_help(la2, lst) }
    }
  }
    
      
  val check: Lambda => Boolean = ( (lam) => { check_help(lam, Nil()) } )
      
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