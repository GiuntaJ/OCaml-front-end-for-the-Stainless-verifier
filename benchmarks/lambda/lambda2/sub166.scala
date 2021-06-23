import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub166 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def chk(a, var0) = {
    a match {
      case Cons(hd, tl) => { if (var0 == hd) true else chk(tl, var0) }
      case _ => { false }
    }
  }   
   
    val lst = Nil()    
  
    def chk2(lst, exp) = {
    exp match {
      case P(a, b) => { chk2(a :: lst, b) }
      case C(a, b) => { if (chk2(lst, a) && chk2(lst, b)) true else false }
      case V(a) => { if (chk(lst, a)) true else false }
    }
  }
  
  
    def check: Exp => Boolean = ( (exp) => { chk2(Nil(), exp) } )
}