import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub134 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  
  def checking(lam: Lambda, lst: List[Var]): Boolean = {
    lst match {
      case Nil() => {
        lam match {
          case V(x) => { false }
          case P(x, l) => { checking(l, x :: lst) }
          case C(l1, l2) => {
            
              if (
                checking(l1, lst) == true && checking(l2, lst) == true
              ) {
                true 
              } else {
                false
              }
          }
        }
      }
      case Cons(hd, tl) => {
        lam match {
          case V(x) => { if (x == hd) true else checking(lam, tl) }
          case P(x, l) => { checking(l, x :: lst) }
          case C(l1, l2) => {
            
              if (
                checking(l1, lst) == true && checking(l2, lst) == true
              ) {
                true 
              } else {
                false
              }
          }
        }
      }
    }
  }
      
      
  def check: Lambda => Boolean = ( (lam) => { checking(lam, Nil()) } )
    
  
  check(P("a", V("a")))
  check(P("a", P("a", V("a"))))
  check(P("a", P("b", C(V("a"), V("b")))))
  check(P("a", C(V("a"), P("b", V("a")))))
  
  check(P("a", V("b")))
  check(P("a", C(V("a"), P("b", V("c")))))
  check(P("a", P("b", C(V("a"), V("c")))))
      
}
