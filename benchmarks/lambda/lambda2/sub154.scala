import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub154 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def allvar: Exp => List[String] = (
    (exp) =>
      {
        exp match {
          case V(v) => { List(v) }
          case P(v, ex) => { Nil() }
          case C(ex1, ex2) => { allvar(ex1) ++ allvar(ex2) }
        }
    }
  )
  
  def searchlist: (List[Var], Var) => Boolean = {
    case (lst, var0) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == var0) true else searchlist(tl, var0)
          }
        }
    }
  }
  
  def complist: (List[Var], List[Var]) => Boolean = {
    case (l1, l2) =>
      {
        l2 match {
          case Nil() => { true }
          case Cons(hd, tl) => {
            if (searchlist(l1, hd)) complist(l1, tl) else false
          }
        }
    }
  }
  
  def checktest: (Exp, List[Var]) => Boolean = {
    case (exp, l) =>
      {
        exp match {
          case V(v) => { true }
          case P(v, ex) => {
            
              if (
                complist(List(v) ++ l, allvar(ex))
              ) {
                checktest(ex, List(v) ++ l) 
              } else {
                false
              }
          }
          case C(ex1, ex2) => { checktest(ex1, l) && checktest(ex2, l) }
        }
    }
  }
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(v) => { false }
          case _ => { checktest(exp, Nil()) }
        }
    }
  ) /* TODO */
}