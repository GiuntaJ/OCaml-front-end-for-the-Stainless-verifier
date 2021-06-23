import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub35 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Lenv = List[Var]
  val empty_lenv: List[A] = Nil()
  def apply_lenv[A](e: List[A], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(y, tl) => { if (x == y) true else apply_lenv(tl, x) }
    }
  }
  
  def pre_check: (Lambda, Lenv) => Boolean = {
    case (ld, le) =>
      {
        ld match {
          case V(x) => { apply_lenv(le, x) }
          case P(v, l) => {
            val _2 = {
              val le_0 = v :: le
              pre_check(l, le_0)
            }
          }
          case C(l1, l2) => { pre_check(l1, le) && pre_check(l2, le) }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (ld) => { pre_check(ld, empty_lenv) } )
}
