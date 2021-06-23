import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub18 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  val addlist: (Exp, List[Var]) => List[Var] = {
    case (e, l) =>
      {
        e match {
          case P(v, e1) => { v :: l }
          case _ => { l }
        }
    }
  }
  
  def check_env: (List[Var], Var) => Boolean = {
    case (l, v) =>
      {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == v) true else check_env(tl, v) }
        }
    }
  }
  
  val var_list: List[Var] = Nil()
  
  def eval: (Exp, List[Var]) => Boolean = {
    case (e, l) =>
      {
        e match {
          case V(var0) => { check_env(l, var0) }
          case P(v, e1) => { eval(e1, addlist(e, l)) }
          case C(e1, e2) => { eval(e1, l) && eval(e2, l) }
        }
    }
  }
  
  def check: Exp => Boolean = ( (e) => { eval(e, var_list) } )
}