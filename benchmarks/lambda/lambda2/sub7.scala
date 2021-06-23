import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub7 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def pcheck: (List[Var], Exp) => Boolean = {
    case (var0, exp) =>
      {
        exp match {
          case V(evar) => { var0.exists(( (v) => { v == evar } )) }
          case P(v, exp) => { pcheck(v :: var0, exp) }
          case C(e1, e2) => { pcheck(var0, e1) && pcheck(var0, e2) }
        }
    }
  }
  
    val check: Exp => Boolean = (
    (e) =>
      {
        e match {
          case V(v) => { false }
          case P(v, exp) => { pcheck(List(v), exp) }
          case C(e1, e2) => { false }
        }
    }
  )
}