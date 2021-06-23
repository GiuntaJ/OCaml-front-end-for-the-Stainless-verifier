import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub51 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
   
     val check: Exp => Boolean = ( (e) => { true } )
   
  def exist[A](v: A, lst: List[A]): Boolean = {
    lst match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (v == hd) true else exist(v, tl) }
    }
  } 
   
  def finding(e: Exp, lst: List[Var]): Boolean = {
    e match {
      case V(v) => { exist(v, lst) }
      case P(v, e) => { finding(e, lst ++ List(v)) }
      case C(e1, e2) => {
        if (finding(e1, lst) && finding(e2, lst)) true else false
      }
    }
  }
   
  def check: Exp => Boolean = ( (e) => { finding(e, Nil()) } )
   
}