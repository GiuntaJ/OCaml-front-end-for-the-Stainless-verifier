import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub126 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  
  	def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(_) => { false }
          case P(a, e) => {
            val _2 = {
              val l = cklist(e)
              l match {
                case Nil() => { true }
                case Cons(hd, tl) => {
                  
                    if (
                      mtlist(a, l)
                    ) {
                      
                        if (
                          remlist(a, l) == Nil()
                        ) {
                          true 
                        } else {
                          e match {
                            case V(_) => { true }
                            case P(_, _) => { check(e) }
                            case C(V(a), V(b)) => { true }
                            case C(V(_), e1) => { check(e1) }
                            case C(e1, V(_)) => { check(e1) }
                            case C(e1, e2) => { check(e) }
                          }
                        } 
                    } else {
                      false
                    }
                }
              }
            }
          }
          case C(e1, e2) => { check(e1) && check(e2) }
        }
    }
  )
  def cklist(exp) = {
    exp match {
      case V(a) => { List(a) }
      case P(a, e) => { cklist(e) }
      case C(e1, e2) => { cklist(e1) ++ cklist(e2) }
    }
  }
  def mtlist(((a, l))) = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == a) true else mtlist(a, tl) }
    }
  }
  def remlist(((a, l))) = {
    l match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        if (hd == a) remlist(a, tl) else List(a) ++ remlist(a, tl)
      }
    }
  }
}
