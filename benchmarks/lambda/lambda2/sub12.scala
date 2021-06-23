import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub12 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def extract: Exp => List[Var] = (
    (e) =>
      {
        e match {
          case V(v) => { Nil() }
          case P(v1, e1) => { List(v1) ++ extract(e1) }
          case C(e1, e2) => { extract(e1) ++ extract(e2) }
        }
    }
  )
  
    def search: (Var, List[Var]) => Boolean = {
    case (v, lst) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(head, Nil()) => { if (v == head) true else false }
          case Cons(head, tail) => {
            if (v == head || search(v, tail)) true else false
          }
        }
    }
  }
   
    def check2: (Exp, List[Var]) => Boolean = {
    case (e, a) =>
      {
        e match {
          case V(v1) => { search(v1, a) }
          case P(v1, e1) => { check2(e1, a) }
          case C(e1, e2) => { check2(e1, a) && check2(e2, a) }
        }
    }
  }
  
    val check: Exp => Boolean = ( (e) => { check2(e, extract(e)) } )
   
}