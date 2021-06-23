import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub17 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def ifexist: (Var, List[String]) => Boolean = {
    case (v, lst) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(h, t) => { if (v == h) true else ifexist(v, t) }
        }
    }
  }
  
  def helpcheck: (Exp, List[String]) => Boolean = {
    case (e, lst) =>
      {
        e match {
          case V(v) => { ifexist(v, lst) }
          case P(v, ex) => { helpcheck(ex, v :: lst) }
          case C(v1, v2) => {
            
              if (
                helpcheck(v1, lst) == true
              ) {
                if (helpcheck(v2, lst) == true) true else false 
              } else {
                false
              }
          }
        }
    }
  }
  
  val check: Exp => Boolean = ( (e) => { helpcheck(e, Nil()) } ) /* TODO */
}