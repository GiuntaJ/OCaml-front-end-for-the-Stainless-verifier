import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub63 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def erase: (Var, List[Var]) => List[Var] = {
    case (v, lst) =>
      {
        lst match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => {
            if (v == hd) erase(v, tl) else hd :: erase(v, tl)
          }
        }
    }
  }
  
  def check_help: Lambda => List[Var] = (
    (lam) =>
      {
        val _4 = {
          val lst = Nil()
          lam match {
            case V(v) => { v :: lst }
            case P(v, l) => { erase(v, check_help(l)) }
            case C(l1, l2) => { check_help(l1) ++ check_help(l2) }
          }
        }
    }
  )
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        lam match {
          case V(v) => { false }
          case P(v, l) => {
            if (erase(v, check_help(l)).length eq 0) true else false
          }
          case C(l1, l2) => { false }
        }
    }
  )
}
