import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub161 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def searchlist: (Var, List[Var]) => Boolean = {
    case (v, lst) =>
      {
        lst match {
          case Cons(hd, tl) => { if (hd == v) true else searchlist(v, tl) }
          case Nil() => { false }
        }
    }
  }
  
  def search: (Exp, List[Var]) => Boolean = {
    case (exp, lst) =>
      {
        exp match {
          case P(a, b) => { search(b, a :: lst) }
          case V(a) => { searchlist(a, lst) }
          case C(a, b) => {
            if (search(b, lst) && search(a, lst)) true else false
          }
        }
    }
  }
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          val lst = Nil()
          exp match {
            case P(a, b) => { search(b, a :: lst) }
            case V(a) => { false }
            case C(a, b) => {
              if (check(a) == true && check(b) == true) true else false
            }
          }
        }
    }
  )
}