import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub3 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def is_well_formed: (Exp, List[Var]) => Boolean = {
    case (e, env) =>
      {
        e match {
          case V(v) => { if (env.contains(v)) true else false }
          case P(v, exp_0) => {
            val _6 = {
              val env_0 = v :: env
              is_well_formed(exp_0, env_0)
            }
          }
          case C(exp1, exp2) => {
            val _2 = {
              val r1 = is_well_formed(exp1, env)
              val _3 = {
                val r2 = is_well_formed(exp2, env)
                r1 && r2
              }
            }
          }
        }
    }
  }
  
    val check: Exp => Boolean = ( (e) => { is_well_formed(e, Nil()) } ) 
}