import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub158 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def has_env: (List[Var], Var) => Boolean = {
    case (li, v) =>
      {
        li match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == v) true else has_env(tl, v) }
        }
    }
  }
  
    def checklist: (Exp, List[Var]) => Boolean = {
    case (exp, l) =>
      {
        exp match {
          case V(v) => { has_env(l, v) }
          case P(v, e) => {
            val _2 = {
              val li = v :: l
              checklist(e, li)
            }
          }
          case C(e1, e2) => { checklist(e1, l) && checklist(e2, l) }
        }
    }
  }
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _7 = {
          val li = Nil()
          checklist(exp, li)
        }
    }
  ) /* TODO */
}