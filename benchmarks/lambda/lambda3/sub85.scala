import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub85 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def bound_var_list(lamlist: List[Lambda], lst: List[Var]): Boolean = {
    lamlist match {
      case Nil() => { true }
      case Cons(hd, tl) => {
        hd match {
          case V(a) => { if (lst.contains(a) == true) true else false }
          case P(a, l) => {
            
              if (
                lst.contains(a) == true
              ) {
                bound_var_list(List(l), lst) 
              } else {
                bound_var_list(List(l), a :: lst)
              }
          }
          case C(l1, l2) => {
            
              if (
                bound_var_list(List(l1), lst) == true
              ) {
                bound_var_list(List(l2), lst) 
              } else {
                false
              }
          }
        }
      }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { bound_var_list(List(lam), Nil()) } )
  
  
  
  
  
  
  
  
}
