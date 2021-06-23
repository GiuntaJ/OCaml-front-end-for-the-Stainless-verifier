import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub30 {
  /*problem 2*/
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def classify(lam: Lambda, li: List[Var]): Boolean = {
    lam match {
      case P(v, l) => { classify(l, li ++ List(v)) }
      case C(l1, l2) => { classify(l1, li) && classify(l2, li) }
      case V(a) => {
        li match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == a) true else classify(lam, tl) }
        }
      }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { classify(lam, Nil()) } ) 
}