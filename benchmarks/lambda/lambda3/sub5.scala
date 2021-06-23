import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub5 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def del[A](x: A, l: List[A]): List[A] = {
    l match {
      case Nil() => { l }
      case Cons(hd, tl) => { if (x == hd) del(x, tl) else hd :: del(x, tl) }
    }
  }
  
  def proc(lambda: Lambda): List[Var] = {
    lambda match {
      case V(x) => { List(x) }
      case P(x, la) => { del(x, proc(la)) }
      case C(la1, la2) => { proc(la1) ++ proc(la2) }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { if (proc(lam) == Nil()) true else false } )
}