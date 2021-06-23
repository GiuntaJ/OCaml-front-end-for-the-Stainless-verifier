import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub58 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def find(x, env) = {
            env match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == x) true else find(x, tl) }
            }
          }
          val _5 = {
            def del(x, env) = {
              env match {
                case Nil() => { Nil() }
                case Cons(v, tl) => { if (v == x) tl else v :: del(x, tl) }
              }
            }
            val _6 = {
              def free(l, env) = {
                l match {
                  case V(v) => { if (find(v, env)) env else v :: env }
                  case P(v, l1) => { del(v, free(l1, env)) }
                  case C(l1, l2) => {
                    val _9 = {
                      val env1 = free(l1, env)
                      free(l2, env1)
                    }
                  }
                }
              }
              val _10 = {
                val env0 = Nil()
                if (free(lam, env0) == Nil()) true else false
              }
            }
          }
        }
    }
  )
}
