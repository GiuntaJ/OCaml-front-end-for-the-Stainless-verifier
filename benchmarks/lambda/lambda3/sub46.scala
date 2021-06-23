import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub46 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Fvset = List[Var]
  def extendfv[A](v: A, l: List[A]): List[A] = { v :: l }
  def xcludefv[A](v: A, l: List[A]): List[A] = {
    l match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        if (hd == v) xcludefv(v, tl) else hd :: xcludefv(v, tl)
      }
    }
  }
  
  def makefvset: (Lambda, Fvset) => Fvset = {
    case (lam, s) =>
      {
        lam match {
          case V(v) => { extendfv(v, s) }
          case P(v, l) => { xcludefv(v, makefvset(l, s)) }
          case C(l1, l2) => { makefvset(l2, s) ++ makefvset(l1, s) }
        }
    }
  }
  
  def isempty[A](l: List[A]): Boolean = {
    l match {
      case Nil() => { true }
      case Cons(h, t) => { false }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { isempty(makefvset(lam, Nil())) } )
}