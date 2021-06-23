import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub1 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def findTarget[A](tar: A, lst: List[A]): Boolean = {
    lst match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == tar) true else findTarget(tar, tl) }
    }
  }
  
  def isBound: (List[Var], Exp) => Boolean = {
    case (x, y) =>
      {
        y match {
          case V(c) => { findTarget(c, x) }
          case P(c, d) => { isBound(c :: x, d) }
          case C(c, d) => { isBound(x, c) && isBound(x, d) }
        }
    }
  }
  def check: Exp => Boolean = ( (e) => { isBound(Nil(), e) } )
}