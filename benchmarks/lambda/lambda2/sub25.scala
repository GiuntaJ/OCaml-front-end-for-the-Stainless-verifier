import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub25 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String 
  
  
  def var0(v: Var): Exp = { V(v) }
    def proc(v, e) = { P(v, e) }
    def call(e1, e2) = { C(e1, e2) }
    
    def free_vars = (
    x =>
      x match {
        case V(v) => { List(v) }
        case P(v, e) => { free_vars(e).filter(( (x) => { x != v } )) }
        case C(e1, e2) => {
          val _5 = {
            val f_e1 = free_vars(e1)
            val _6 = {
              val f_e2 = free_vars(e2)
              f_e1 ++(f_e2.filter(( (x) => { not(f_e1.contains(x)) } )))
            }
          }
        }
      }
  )
  
    def fresh_var(v1, l) = { if (l.contains(v1)) fresh_var(v1 + "'", l) else v1 }
  
    val check: Exp => Boolean = ( (e) => { if (free_vars(e) == Nil()) true else false } )
}