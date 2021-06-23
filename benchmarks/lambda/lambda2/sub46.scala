import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub46 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def insert(a, l) = {
    l match {
      case Nil() => { List(a) }
      case Cons(hd, tl) => { a :: hd :: tl }
    }
  }
  
    def confirm(a, l) = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == a) true else confirm(a, tl) }
    }
  }
  
    def cal: (List[String], Exp) => List[String] = {
    case (a, b) =>
      {
        b match {
          case V(v) => { if (confirm(v, a)) a else insert("false", a) }
          case P(x, y) => {
            if (confirm(x, a)) cal(a, y) else cal(insert(x, a), y)
          }
          case C(x, y) => { cal(cal(a, x), y) }
        }
    }
  }
  
    val check: Exp => Boolean = (
    (e) =>
      {
        e match {
          case V(x) => { false }
          case P(x, y) => {
            val _2 = {
              val l = cal(insert(x, Nil()), y)
              if (confirm("false", l)) false else true
            }
          }
          case C(x, y) => { false }
        }
    }
  ) 
   
}