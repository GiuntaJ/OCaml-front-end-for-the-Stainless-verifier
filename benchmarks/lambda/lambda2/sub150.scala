import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub150 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def ck: (Exp, List[String]) => Int63 = {
    case (exp, lst) =>
      {
        exp match {
          case P(va, ex) => { ck(ex, va :: lst) }
          case C(ex1, ex2) => { ck(ex1, lst) + ck(ex2, lst) }
          case V(va) => { if (lst.contains(va) == true) 0 else 1 }
        }
    }
  } 
  
  
  
  
  /*=====================================================*/
  /*스타또!*/
  /*=====================================================*/
    val check: Exp => Boolean = ( (exp) => { if (ck(exp, Nil()) == 0) true else false } )
}
