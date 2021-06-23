import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub155 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def comparelist: (List[A], List[A]) => List[A] = {
    case (procdata, vardata) =>
      {
        (procdata, vardata) match {
          case (_, Nil()) => { Nil() }
          case (Cons(hd1, tl1), Cons(hd2, tl2)) => {
            
              if (
                hd1 == hd2
              ) {
                comparelist(procdata, tl2) 
              } else {
                comparelist(List(hd1), tl2) ++ comparelist(tl1, vardata)
              }
          }
          case (Nil(), Cons(hd2, tl2)) => { hd2 :: tl2 }
        }
    }
  } 
  	
    def varlist: Exp => List[Var] = (
    (exp) =>
      {
        exp match {
          case V(v) => { List(v) }
          case P(_, a) => { varlist(a) }
          case C(a, b) => { varlist(a) ++ varlist(b) }
        }
    }
  )
  
    def proclist: Exp => List[Var] = (
    (exp) =>
      {
        exp match {
          case P(v, a) => { v :: proclist(a) }
          case C(a, b) => { proclist(a) ++ proclist(b) }
          case _ => { Nil() }
        }
    }
  )
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          val list1 = proclist(exp)
          val _5 = {
            val list2 = varlist(exp)
            val _6 = {
              val list3 = comparelist(list1, list2)
              if (list3 == Nil()) true else false
            }
          }
        }
    }
  )
}