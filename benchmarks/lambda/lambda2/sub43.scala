import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub43 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def ch(((e, l))) = {
    e match {
      case V(v) => { if (l.contains(v)) true else false }
      case P(v, ex) => { ch(ex, l ++ List(v)) }
      case C(e1, e2) => { ch(e1, l) && ch(e2, l) }
    }
  }
  
  def check(e: Exp): Boolean = { ch(e, Nil()) }
}