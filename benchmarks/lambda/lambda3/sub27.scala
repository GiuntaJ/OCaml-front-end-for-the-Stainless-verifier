import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub27 {
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
  def apply_bound: (Var, Bound) => Boolean = {
    case (v, bnd) =>
      {
        bnd match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == v) true else apply_bound(v, tl) }
        }
    }
  }
  
  val extend_bound: (Var, Bound) => Bound = {
    case (v, bnd) => { v :: bnd }
  }
  def sub_check: (Lambda, Bound) => Boolean = {
    case (lam, bnd) =>
      {
        lam match {
          case V(v) => { apply_bound(v, bnd) }
          case P(v, lamb) => {
            val _2 = {
              val bnd_n = extend_bound(v, bnd)
              sub_check(lamb, bnd_n)
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
}