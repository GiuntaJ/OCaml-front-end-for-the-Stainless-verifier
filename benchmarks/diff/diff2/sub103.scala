import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub103 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
    
    def cal_diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(x) => { if (x == var0) Const(1) else Const(0) }
          case Power(x, n) => {
            
              if (
                x == var0
              ) {
                n match {
                  case 0 => { Const(0) }
                  case 1 => { Const(1) }
                  case 2 => { Times(List(Const(2), Var(var0))) }
                  case _ => { Times(List(Const(n), Power(var0, n - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Times(Cons(hd, tl)) => {
            
              if (
                tl == Nil()
              ) {
                cal_diff(hd, var0) 
              } else {
                Sum(
                  List(Times(List(cal_diff(hd, var0)) ++ tl),
                   Times(List(hd) ++ List(cal_diff(Times(tl), var0)))))
              }
          }
          case Sum(Cons(hd, tl)) => {
            
              if (
                tl == Nil()
              ) {
                cal_diff(hd, var0) 
              } else {
                Sum(List(cal_diff(hd, var0)) ++ List(cal_diff(Sum(tl), var0)))
              }
          }
          case Times(Nil()) => { Const(0) }
          case Sum(Nil()) => { Const(0) }
        }
    }
  }
    
    val diff: (Aexp, String) => Aexp = {
    case (exp, var0) => { cal_diff(exp, var0) }
  }
}