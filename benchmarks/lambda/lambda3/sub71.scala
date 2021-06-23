import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub71 {
  /*********************/
  /* Problem 2*/
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def find2[A](v: A, m: List[A]): Boolean = {
    m match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == v) true else find2(v, tl) }
    }
  }
  
  def addMem2[A](v: A, m: List[A]): List[A] = { v :: m }
  
  def check2: (Lambda, List[String]) => Boolean = {
    case (lam, mem) =>
      {
        lam match {
          case V(v) => { find2(v, mem) }
          case P(v, l) => {
            val _2 = {
              val mem2 = addMem2(v, mem)
              check2(l, mem2)
            }
          }
          case C(l1, l2) => { check2(l1, mem) && check2(l2, mem) }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { check2(lam, Nil()) } )
}