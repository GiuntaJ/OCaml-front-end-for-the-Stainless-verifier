import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub112 {
  
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  
  def fv(((va, la))) = {
    la match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (va == hd) true else fv(va, tl) }
    }
  }
  
  def findv(((a, la))) = {
    a match {
      case V(b) => { fv(b, la) }
      case P(va, a1) => { findv(a1, la ++ List(va)) }
      case C(a1, a2) => {
        if ((findv(a1, la) && findv(a2, la)) == true) true else false
      }
    }
  }
  
  val check: Exp => Boolean = ( (a) => { findv(a, Nil()) } )
}