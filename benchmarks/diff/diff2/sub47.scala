import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub47 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, dif_var) =>
      {
        aexp match {
          case Const(n) => { Const(0) }
          case Var(var0) => { if (var0 == dif_var) Const(1) else Const(0) }
          case Power(var0, n) => {
            
              if (
                var0 == dif_var
              ) {
                
                  if (
                    n > 1
                  ) {
                    Times(List(Const(n), Power(var0, n - 1))) 
                  } else if (
                    n == 1
                  ) {
                    Const(1) 
                  } else {
                    Const(0)
                  } 
              } else {
                Const(0)
              }
          }
          case Times(aexp_list) => {
            aexp_list match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                
                  if (
                    tl == Nil()
                  ) {
                    diff(hd, dif_var) 
                  } else {
                    Sum(
                      List(Times(diff(hd, dif_var) :: tl),
                       Times(List(hd, diff(Times(tl), dif_var)))))
                  }
              }
            }
          }
          case Sum(aexp_list) => {
            aexp_list match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                
                  if (
                    tl == Nil()
                  ) {
                    diff(hd, dif_var) 
                  } else {
                    Sum(List(diff(hd, dif_var), diff(Sum(tl), dif_var)))
                  }
              }
            }
          }
        }
    }
  }
}