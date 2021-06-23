import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub48 {
     sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
     def search(x, e) = {
    e match {
      case Nil() => { false }
      case Cons(h, t) => { if (h == x) true else search(x, t) }
    }
  }
  
  
     def sub_check: (Exp, List[String]) => Boolean = {
    case (e, l) =>
      {
        e match {
          case V(v) => { search(v, l) }
          case P(v, e1) => { sub_check(e1, v :: l) }
          case C(e1, e2) => { sub_check(e1, l) && sub_check(e2, l) }
        }
    }
  }
  
     val check: Exp => Boolean = ( (e) => { sub_check(e, Nil()) } )
}