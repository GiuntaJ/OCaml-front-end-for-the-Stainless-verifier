import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub169 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def save: Exp => List[Var] = (
    (exp) =>
      {
        exp match {
          case V(a) => { Nil() }
          case P(a, b) => { List(a) }
          case C(a, b) => { save(a) ++ save(b) }
        }
    }
  )
  
  	def search: (Exp, List[Var]) => Boolean = {
    case (exp, all) =>
      {
        exp match {
          case V(a) => {
            all match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == a) true else search(V(a), tl) }
            }
          }
          case P(a, b) => { search(b, a :: all) }
          case C(a, b) => { search(a, all) && search(b, all) }
        }
    }
  }
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(a) => { false }
          case P(a, b) => { search(b, List(a)) }
          case C(a, b) => { check(a) && check(b) }
        }
    }
  )
}