import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub31 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def free_var(e) = {
    e match {
      case V(v) => { List(v) }
      case P(v, t) => { free_var(t).filter(( (x) => { x != v } )) }
      case C(t, u) => {
        val _2 = {
          val f_t = free_var(t)
          val _3 = {
            val f_u = free_var(u)
            f_t ++(f_u.filter(( (x) => { not(f_t.contains(x)) } )))
          }
        }
      }
    }
  }
  
    val check: Exp => Boolean = (
    (e) =>
      {
        free_var(e) match {
          case Nil() => { true }
          case Cons(_, _) => { false }
        }
    }
  )
}