import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub108 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def forcheck(((ex, l))) = {
    ex match {
      case V(v) => {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (v == hd) true else forcheck(V(v), tl) }
        }
      }
      case P(v, e1) => { forcheck(e1, v :: l) }
      case C(e1, e2) => {
        if (forcheck(e1, l) == true && forcheck(e2, l) == true) true else false
      }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { forcheck(exp, Nil()) } )
}