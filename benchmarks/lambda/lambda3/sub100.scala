import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub100 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val t1: Lambda = P("a", V("a"))
  val t2: Lambda = P("a", P("a", V("a")))
  val t3: Lambda = P("a", P("b", C(V("a"), V("b"))))
  val t4: Lambda = P("a", C(V("a"), P("b", V("a"))))
  
  val t5: Lambda = P("a", V("b"))
  val t6: Lambda = P("a", C(V("a"), P("b", V("c"))))
  val t7: Lambda = P("a", P("b", C(V("a"), V("c"))))
  
  def make_Plst(lam: Lambda): List[Var] = {
    lam match {
      case P(v, lam_0) => { v :: make_Plst(lam_0) }
      case C(lam1, lam2) => { make_Plst(lam1) ++ make_Plst(lam2) }
      case _ => { Nil() }
    }
  }
      
  def make_Vlst(lam: Lambda): List[Var] = {
    lam match {
      case V(v) => { List(v) }
      case P(v, lam_0) => { make_Vlst(lam_0) }
      case C(lam1, lam2) => { make_Vlst(lam1) ++ make_Vlst(lam2) }
    }
  }
      
  def check_in_lst[A](lst: List[A], v: A): Boolean = {
    lst match {
      case Nil() => { false }
      case Cons(head, tail) => { if (head == v) true else check_in_lst(tail, v)
      }
    }
  }
      
  def check_VP[A](lstP: List[A], lstV: List[A]): Boolean = {
    lstV match {
      case Nil() => { true }
      case Cons(head, tail) => {
        if (check_in_lst(lstP, head) == true) check_VP(lstP, tail) else false
      }
    }
  }
      
    
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          val lstP = make_Plst(lam)
          val _5 = {
            val lstV = make_Vlst(lam)
            check_VP(lstP, lstV)
          }
        }
    }
  )
}
