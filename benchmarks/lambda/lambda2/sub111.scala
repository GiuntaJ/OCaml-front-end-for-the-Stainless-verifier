import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub111 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def isin: (Var, List[Var]) => Boolean = {
    case (str, lst) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == str) true else isin(str, tl) }
        }
    }
  }
    
    def filter(lst, lst2) = {
    lst match {
      case Nil() => { true }
      case Cons(hd, tl) => {
        if (isin(hd, lst2) == true) filter(tl, lst2) else false
      }
    }
  }
  
    def bool_check: (Exp, List[Var]) => List[Var] = {
    case (exp, lst) =>
      {
        exp match {
          case V(str) => { lst }
          case P(e1, e2) => { bool_check(e2, List(e1) ++ lst) }
          case C(e1, e2) => { bool_check(e1, lst) ++ bool_check(e2, lst) }
        }
    }
  }
    
    def cal_check: (Exp, List[Var]) => List[Var] = {
    case (exp, lst) =>
      {
        exp match {
          case V(str) => { str :: lst }
          case P(e1, e2) => { cal_check(e2, lst) }
          case C(e1, e2) => { cal_check(e1, lst) ++ cal_check(e2, lst) }
        }
    }
  }
    
    val check: Exp => Boolean = (
    (exp) =>
      {
        
          if (
            filter(cal_check(exp, Nil()), bool_check(exp, Nil())) == true
          ) {
            true 
          } else {
            false
          }
    }
  )
}