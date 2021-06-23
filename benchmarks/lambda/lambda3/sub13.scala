import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub13 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = ( (lam) => { true } ) /* TODO */
       def check_2(((lam, l))) = {
    lam match {
      case V(v) => {
        l match {
          case Nil() => { false }
          case Cons(h, t) => { if (v == h) true else check_2(V(v), t) }
        }
      }
      case P(v, lam) => { check_2(lam, v :: l) }
      case C(lam1, lam2) => {
        
          if (
            check_2(lam1, l) == true && check_2(lam2, l) == true
          ) {
            true 
          } else {
            false
          }
      }
    }
  }
  
      val check: Lambda => Boolean = ( (lam) => { check_2(lam, Nil()) } ) 
}