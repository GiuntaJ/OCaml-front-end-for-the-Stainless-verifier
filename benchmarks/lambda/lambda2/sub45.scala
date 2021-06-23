import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub45 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def check: Exp => Boolean = ( (e) => { checking(e, makelist(e, Nil())) } )
  def makelist: (Exp, List[Var]) => List[Var] = {
    case (e, l) =>
      {
        (e, l) match {
          case (V(v), l) => { l }
          case (P(v, e), l) => { v :: makelist(e, l) }
          case (C(e1, e2), l1) => {
            val _2 = {
              val l2 = makelist(e1, l1)
              makelist(e2, l2)
            }
          }
        }
    }
  }
  def checking: (Exp, List[Var]) => Boolean = {
    case (e, l) =>
      {
        (e, l) match {
          case (V(v), l) => { findv(v, l) }
          case (P(v, e), l) => { checking(e, l) }
          case (C(e1, e2), l) => { checking(e1, l) && checking(e2, l) }
        }
    }
  }
  def findv: (Var, List[Var]) => Boolean = {
    case (v, l) =>
      {
        (v, l) match {
          case (_, Nil()) => { false }
          case (v, Cons(hd, tl)) => { if (hd == v) true else findv(v, tl) }
        }
    }
  }
}
