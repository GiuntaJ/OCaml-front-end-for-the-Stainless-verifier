import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub118 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def find_match: (String, List[String]) => Boolean = {
    case (vr, vr_list) =>
      {
        vr_list match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (vr == hd) true else find_match(vr, tl) }
        }
    }
  } /*list check*/
  
  def vr_match: (Exp, List[String]) => Boolean = {
    case (exp, vr_list) =>
      {
        exp match {
          case V(vr) => { find_match(vr, vr_list) }
          case P(vr, e1) => { vr_match(e1, vr :: vr_list) }
          case C(e1, e2) => {
            if (vr_match(e1, vr_list) && vr_match(e2, vr_list)) true else false
          }
        }
    }
  }
  
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(vr) => { false }
          case P(vr, e1) => { vr_match(e1, List(vr)) }
          case C(e1, e2) => { vr_match(e1, Nil()) && vr_match(e2, Nil()) }
        }
    }
  )
}