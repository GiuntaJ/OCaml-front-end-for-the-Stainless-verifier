import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub34 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Lenv = List[Var] 
  
  /*environment*/
  val empty_lenv: List[A] = Nil()
  def extend_lenv[A](v: A, e: List[A]): List[A] = { v :: e }
  def apply_lenv[A](e: List[A], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(v, tl) => { if (x == v) true else apply_lenv(tl, x) }
    }
  }
  
  def pcheck: (Lambda, Lenv) => Boolean = {
    case (lam, e) =>
      {
        lam match {
          case V(v) => { if (apply_lenv(e, v)) true else false }
          case P(v, l) => {
            val _2 = {
              val e1 = extend_lenv(v, e)
              pcheck(l, e1)
            }
          }
          case C(l1, l2) => {
            if (pcheck(l1, e) && pcheck(l2, e)) true else false
          }
        }
    }
  } 
  
  def check: Lambda => Boolean = ( (lam) => { pcheck(lam, Nil()) } )
}