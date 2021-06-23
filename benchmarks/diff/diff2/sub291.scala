import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub291 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(i) => { Const(0) }
          case Var(s) => { if (s == x) Const(1) else Var(s) }
          case Power(s, i) => {
            
              if (
                s == x
              ) {
                
                  if (
                    i > 2
                  ) {
                    Times(List(Const(i), Power(s, i - 1))) 
                  } else {
                    Times(List(Const(i), Var(s)))
                  } 
              } else {
                Power(s, i)
              }
          }
          case Times(al) => {
            val _6 = {
              def timeiter(lst) = {
                lst match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => {
                    (hd match {
                       case Const(i1) => { Const(i1) }
                       case _ => { diff(hd, x) }
                     }) ::
                    timeiter(tl)
                  }
                }
              }
              Times(timeiter(al))
            }
          }
          case Sum(al) => {
            val _2 = {
              def sumeval(lst) = {
                lst match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => {
                    if (hd == Const(0)) sumeval(tl) else hd :: sumeval(tl)
                  }
                }
              }
              val _3 = {
                def sumiter(lst) = {
                  lst match {
                    case Nil() => { Nil() }
                    case Cons(hd, tl) => { diff(hd, x) :: sumiter(tl) }
                  }
                }
                Sum(sumeval(sumiter(al)))
              }
            }
          }
        }
    }
  }
  
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}