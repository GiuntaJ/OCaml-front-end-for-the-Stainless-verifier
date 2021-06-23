import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub64 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check_occurence(v, l) = {
    l match {
      case Cons(hd, tl) => { if (hd == v) true else check_occurence(v, tl) }
      case Nil() => { false }
    }
  }
  
  def check_form: (Lambda, List[Var]) => Boolean = {
    case (lam, l) =>
      {
        lam match {
          case P(v, lambda1) => { check_form(lambda1, List(v) ++ l) }
          case V(v) => { check_occurence(v, l) }
          case C(lambda1, lambda2) => {
            check_form(lambda1, l) && check_form(lambda2, l)
          }
          case _ => { false }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { check_form(lam, Nil()) } )
  
}
