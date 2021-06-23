import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub76 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  
    val diff: (Aexp, String) => Aexp = {
    case (exp, var0) => { exp }
  }
  	
  	def diff2: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Sum(s) => {
            s match {
              case Nil() => { Const(0) }
              case Cons(hd, Nil()) => { diff2(hd, var0) }
              case Cons(hd, tl) => {
                Sum(List(diff2(hd, var0), diff2(Sum(tl), var0)))
              }
            }
          }
          case Times(s) => {
            s match {
              case Nil() => { Const(1) }
              case Cons(hd, Nil()) => { diff2(hd, var0) }
              case Cons(hd, tl) => {
                hd match {
                  case Const(n) => {
                    Times(List(Const(n), diff2(Times(tl), var0)))
                  }
                  case _ => {
                    Sum(
                      List(Times(List(hd, diff2(Times(tl), var0))),
                       Times(diff2(hd, var0) :: tl)))
                  }
                }
              }
            }
          }
          case Power(s, n) => {
            
              if (
                s == var0
              ) {
                n match {
                  case 0 => { Const(0) }
                  case 1 => { Const(1) }
                  case _ => { Times(List(Const(n), Power(s, n - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Const(n) => { Const(0) }
          case Var(x) => {
            
              if (
                x == var0
              ) {
                Sum(List(Const(1), Times(List(Var(x), Const(0))))) 
              } else {
                Const(0)
              }
          }
        }
    }
  }
  	val diff: (Aexp, String) => Aexp = {
    case (exp, var0) => { diff2(exp, var0) }
  }
}