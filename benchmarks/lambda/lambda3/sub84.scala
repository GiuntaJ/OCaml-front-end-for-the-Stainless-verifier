import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub84 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  
  type T = List[Var]
  val empty: List[A] = Nil()
  
  val push: (Var, T) => T = {
    case (v, stack) => { v :: stack }
  }
    
  def lookup_stack: (Var, T) => Boolean = {
    case (v, stack) =>
      {
        stack match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (v == hd) true else lookup_stack(v, tl) }
        }
    }
  }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def find_v: (Lambda, T) => Boolean = {
            case (lam, stack) =>
              {
                lam match {
                  case V(v) => { lookup_stack(v, stack) }
                  case P(v, l) => {
                    val _7 = {
                      val stack_0 = push(v, stack)
                      find_v(l, stack_0)
                    }
                  }
                  case C(l1, l2) => { find_v(l1, stack) && find_v(l2, stack) }
                }
            }
          }
          find_v(lam, empty)
        }
    }
  )
}