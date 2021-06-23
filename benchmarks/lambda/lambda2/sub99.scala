import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub99 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  	  
  	  def bound: Exp => List[Var] = (
    (exp) =>
      {
        exp match {
          case V(var0) => { Nil() }
          case P(var0, ex) => { var0 :: bound(ex) }
          case C(ex1, ex2) => { bound(ex1) ++ bound(ex2) }
        }
    }
  )
  	  
  	  def variables: Exp => List[Var] = (
    (exp) =>
      {
        exp match {
          case V(var0) => { List(var0) }
          case P(var0, ex) => { variables(ex) }
          case C(ex1, ex2) => { variables(ex1) ++ variables(ex2) }
        }
    }
  )
  	  
  	  def containHelper: (List[Var], Var) => Boolean = {
    case (bound, a) =>
      {
        (bound, a) match {
          case (Nil(), a) => { false }
          case (Cons(x, tl), a) => { if (x == a) true else containHelper(tl, a)
          }
        }
    }
  }
  	  
  	  def contain: (List[Var], List[Var]) => Boolean = {
    case (bound, variables) =>
      {
        (bound, variables) match {
          case (Nil(), variables) => { false }
          case (bound, Nil()) => { true }
          case (bound, Cons(a, tl)) => {
            if (containHelper(bound, a) == false) false else contain(bound, tl)
          }
        }
    }
  }
  
  	  val check: Exp => Boolean = ( (exp) => { contain(bound(exp), variables(exp)) } )
}