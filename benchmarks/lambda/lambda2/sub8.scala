import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub8 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def look: (Var, List[String]) => Boolean = {
    case (v, li) =>
      {
        li match {
          case Nil() => { false }
          case Cons(h, t) => { if (h == v) true else look(v, t) }
        }
    }
  }
  
  def add: (Var, List[String]) => List[String] = {
    case (v, li) =>
      {
        li match {
          case Nil() => { li ++ List(v) }
          case Cons(h, t) => { if (h == v) li else List(h) ++ add(v, t) }
        }
    }
  }
  
  def collect1: (Exp, List[String]) => List[String] = {
    case (e, li) =>
      {
        e match {
          case P(v, e1) => { collect1(e1, add(v, li)) }
          case C(e1, e2) => { collect1(e2, collect1(e1, li)) }
          case V(v1) => { li }
        }
    }
  } 
  
  def collect2: (Exp, List[String]) => List[String] = {
    case (e, li) =>
      {
        e match {
          case P(v, e1) => { collect2(e1, li) }
          case C(e1, e2) => { collect2(e2, collect2(e1, li)) }
          case V(v1) => { add(v1, li) }
        }
    }
  }
  
  def compare: (List[String], List[String]) => Boolean = {
    case (li1, li2) =>
      {
        li2 match {
          case Nil() => { true }
          case Cons(h, t) => { look(h, li1) && compare(li1, t) }
        }
    }
  }
  
  val check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          val li1 = collect1(e, Nil())
          val _5 = {
            val li2 = collect2(e, Nil())
            compare(li1, li2)
          }
        }
    }
  )
}