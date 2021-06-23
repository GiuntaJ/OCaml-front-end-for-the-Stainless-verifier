import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub98 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def makePList(exp, pl) = {
    exp match {
      case V(v) => { pl }
      case P(v, e) => { pl ++ List(v) ++ makePList(e, pl) }
      case C(e1, e2) => { makePList(e1, pl) ++ makePList(e2, pl) }
    }
  }
  
    def makeVList(exp, vl) = {
    exp match {
      case V(v) => { vl ++ List(v) }
      case P(v, e) => { vl ++ makeVList(e, vl) }
      case C(e1, e2) => { makeVList(e1, vl) ++ makeVList(e2, vl) }
    }
  }
  
    def compareToPlist(pl, e) = {
    pl match {
      case Nil() => { false }
      case Cons(hd, tl) => { e == hd || compareToPlist(tl, e) }
    }
  }
  
    def compareList(pl, vl) = {
    vl match {
      case Nil() => { true }
      case Cons(hd, tl) => { compareList(pl, tl) && compareToPlist(pl, hd) }
    }
  }
  
  
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(v) => { false }
          case P(v, e) => {
            val _2 = {
              val pl = makePList(exp, Nil())
              val _3 = {
                val vl = makeVList(exp, Nil())
                compareList(pl, vl)
              }
            }
          }
          case C(e1, e2) => { check(e1) && check(e2) }
        }
    }
  )
}