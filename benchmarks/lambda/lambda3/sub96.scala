import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub96 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def find_var: (Var, List[Var]) => List[Var] = {
    case (x, lst) =>
      {
        lst match {
          case Nil() => { Nil() }
          case Cons(h, t) => {
            if (h == x) find_var(x, t) else h :: find_var(x, t)
          }
        }
    }
  }
  
  def eval: Lambda => List[Var] = (
    (lam) =>
      {
        lam match {
          case V(x) => { List(x) }
          case P(x, l) => { find_var(x, eval(l)) }
          case C(l1, l2) => { eval(l1) ++ eval(l2) }
        }
    }
  )
  
  val check: Lambda => Boolean = ( (lam) => { if (eval(lam) == Nil()) true else false } )
    
}