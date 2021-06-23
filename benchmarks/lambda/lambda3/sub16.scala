import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub16 {
    /*********************/
    /*   Problem 2        */
    /*********************/
  
    sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val extend1: (String, List[String]) => List[String] = {
    case (x, l) => { x :: l }
  }
  def find1: (String, List[String]) => Boolean = {
    case (x, l) =>
      {
        l match {
          case Cons(hd, tl) => { if (hd == x) true else find1(x, tl) }
          case _ => { false }
        }
    }
  }
  
  def hcheck: (Lambda, List[String]) => Boolean = {
    case (lam, s) =>
      {
        lam match {
          case V(x) => { find1(x, s) }
          case P(x, l) => {
            val _2 = {
              val s1 = extend1(x, s)
              hcheck(l, s1)
            }
          }
          case C(l1, l2) => { hcheck(l1, s) && hcheck(l2, s) }
        }
    }
  }
  
  
  
  def check: Lambda => Boolean = ( (lam) => { hcheck(lam, Nil()) } ) 
  
  
  
}
