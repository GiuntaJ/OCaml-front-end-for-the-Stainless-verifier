import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub124 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Tenv = Var => Boolean
  val empty_tenv: A => Boolean = ( (y) => { false } )
  def extend(x, tenv, y) = { if (x == y) true else tenv(y) }
  
  def check_aux: (Tenv, Lambda) => Boolean = {
    case (tenv, lam) =>
      {
        lam match {
          case V(a) => { tenv(a) }
          case P(a, lam) => { check_aux(extend(a, tenv), lam) }
          case C(lam1, lam2) => { check_aux(tenv, lam1) && check_aux(tenv, lam2)
          }
        }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { check_aux(empty_tenv, lam) } )
}
