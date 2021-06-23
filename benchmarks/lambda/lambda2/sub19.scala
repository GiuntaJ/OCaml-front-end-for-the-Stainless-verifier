import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub19 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def checklist: (List[Var], Var) => Boolean = {
    case (lst, x) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (x eq hd) true else checklist(tl, x) }
        }
    }
  }
  			
  		
  def makelist: (List[Var], Exp) => Boolean = {
    case (lst, e) =>
      {
        e match {
          case V(x) => { checklist(lst, x) }
          case P(a, e) => { makelist(lst ++ List(a), e) }
          case C(e1, e2) => { makelist(lst, e1) && makelist(lst, e2) }
        }
    }
  }
  		
    
  def check: Exp => Boolean = ( (e) => { makelist(Nil(), e) } )   
  
}
