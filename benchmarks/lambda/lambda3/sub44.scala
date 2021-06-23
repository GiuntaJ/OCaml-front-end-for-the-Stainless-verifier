import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub44 {
  			
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Variables = List[Var] /*necessary?? var?? */
  
  def findVar: (List[Var], Lambda) => Boolean = {
    case (vlist, lam) =>
      {
        lam match {
          case V(x) => {
            vlist match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == x) true else findVar(tl, lam) }
            }
          }
          case P(v, l) => { findVar(v :: vlist, l) }
          case C(l1, l2) => { findVar(vlist, l1) && findVar(vlist, l2) }
        }
    }
  }
  	
  def check: Lambda => Boolean = ( (lam) => { findVar(Nil(), lam) } )
  
  /* 
  check (P ("a", V "a"));;
  check (P ("a", P ("a", V "a")));;
  check (P ("a", P ("b", C (V "a", V "b"))));;
  check (P ("a", C (V "a", P ("b", V "a"))));;
  check (P ("a", V "b"));;
  check (P ("a", C (V "a", P ("b", V "c"))));;
  check (P ("a", P ("b", C (V "a", V "c"))));;
  */
}