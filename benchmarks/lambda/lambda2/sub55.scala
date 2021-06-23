import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub55 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
  
  
  def check(a: Exp): Boolean = {
    val _2 = {
      def isinlst(l, x) = {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == x) true else isinlst(tl, x) }
        }
      }
      val _3 = {
        def letsdoit(e, lst) = {
          e match {
            case V(i) => { isinlst(lst, i) }
            case P(i, j) => { letsdoit(j, i :: lst) }
            case C(i, j) => { letsdoit(i, lst) && letsdoit(j, lst) }
          }
        }
        letsdoit(a, Nil())
      }
    }
  }
}