import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub103 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def lookup_env[A](x: A, e: List[A]): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(y, tl) => { if (x == y) true else lookup_env(x, tl) }
    }
  }
  
  def checkhel: (Lambda, List[Var]) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(v) => { lookup_env(v, env) }
          case P(v, l) => { checkhel(l, v :: env) }
          case C(l1, l2) => {
            
              if (
                checkhel(l1, env) == true && checkhel(l2, env) == true
              ) {
                true 
              } else {
                false
              }
          }
        }
    }
  }
    
    
  val check: Lambda => Boolean = ( (lam) => { checkhel(lam, Nil()) } ) 
  
  check(P("a", V("a")))
  check(P("a", P("a", V("a"))))
  check(P("a", P("b", C(V("a"), V("b")))))
  check(P("a", C(V("a"), P("b", V("a")))))
  check(P("a", V("b")))
  check(P("a", C(V("a"), P("b", V("c")))))
  check(P("a", P("b", C(V("a"), V("c")))))
}