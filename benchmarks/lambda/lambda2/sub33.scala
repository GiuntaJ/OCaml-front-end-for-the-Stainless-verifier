import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub33 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
   
  	def checklist: (String, List[String]) => Boolean = {
    case (s, l) =>
      {
        l match {
          case Cons(hd, tl) => { if (hd == s) true else checklist(s, tl) }
          case Nil() => { false }
        }
    }
  }
  
  	def parse: (Exp, List[String]) => Boolean = {
    case (e, l) =>
      {
        e match {
          case V(a) => { checklist(a, l) }
          case P(v, e) => { parse(e, v :: l) }
          case C(e1, e2) => { parse(e1, l) && parse(e2, l) }
        }
    }
  }
  	
    val check: Exp => Boolean = ( (e) => { parse(e, Nil()) } )
}