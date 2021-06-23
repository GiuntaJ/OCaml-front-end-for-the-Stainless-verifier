import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub29 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  type Bound = List[Var]
  val empty_bound: List[A] = Nil()
  def apply_bound: (Var, List[Var]) => Boolean = {
    case (x, bnd) =>
      {
        bnd match {
          case Nil() => { false }
          case Cons(a, tl) => { if (a == x) true else apply_bound(x, tl) }
        }
    }
  }
  val update_bound: (Var, List[Var]) => List[Var] = {
    case (x, bnd) => { x :: bnd }
  }  
  def sub_check: (Lambda, Bound) => Boolean = {
    case (lam, bnd) =>
      {
        lam match {
          case V(x) => { apply_bound(x, bnd) }
          case P(x, lamb) => {
            val _2 = {
              val bnd1 = update_bound(x, bnd)
              sub_check(lamb, bnd1)
            }
          }
          case C(lamb1, lamb2) => {
            
              if (
                sub_check(lamb1, bnd) == true && sub_check(lamb2, bnd) == true
              ) {
                true 
              } else {
                false
              }
          }
        }
    }
  }
  def check: Lambda => Boolean = ( (lam) => { sub_check(lam, Nil()) } )
   /* TODO */
}
