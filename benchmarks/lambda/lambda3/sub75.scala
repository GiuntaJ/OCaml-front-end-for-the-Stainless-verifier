import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub75 {
  /* Problem 2 */ /*********************/ 
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def helper: (Var, List[Var]) => Boolean = {
    case (v, vlist) =>
      {
        vlist match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (v == hd) true else helper(v, tl) }
        }
    }
  }
  
  def checker: (Lambda, List[Var], Boolean) => Boolean = {
    case (l, lst, b) =>
      {
        l match {
          case V(x) => { helper(x, lst) }
          case P(v1, l1) => { checker(l1, v1 :: lst, b) && b }
          case C(l1, l2) => { (checker(l1, lst, b) && checker(l2, lst, b)) && b
          }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (l) => { checker(l, Nil(), true) } )
}