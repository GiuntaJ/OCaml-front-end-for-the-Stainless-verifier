import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub188 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  
  
  def chuchul[A](lst: List[A], x: A): List[A] = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        if (hd == x) chuchul(tl, x) else hd :: chuchul(tl, x)
      }
    }
  }
  
  def diff2(((e, x)), count, sumcount, ls) = {
    e match {
      case Times(lst) => {
        lst match {
          case Nil() => { Times(List(Const(count))) }
          case Cons(hd, tl) => {
            hd match {
              case Const(n) => {
                Times(
                  List(Const(n),
                   diff2(Times(tl), x, count, sumcount, Const(n) :: ls)))
              }
              case Var(y) => {
                
                  if (
                    y == x
                  ) {
                    
                      if (
                        count == 0
                      ) {
                        Times(
                          List(Const(1),
                           diff2(
                             Times(tl), x, count + 1, sumcount, Var(x) :: ls))) 
                      } else {
                        Times(
                          List(Var(x),
                           diff2(
                             Times(tl), x, count + 1, sumcount, Var(x) :: ls)))
                      } 
                  } else {
                    Times(
                      List(Var(y),
                       diff2(Times(tl), x, count, sumcount, Var(y) :: ls)))
                  }
              }
              case Power(y, n) => {
                
                  if (
                    y == x
                  ) {
                    
                      if (
                        count == 0
                      ) {
                        Times(
                          List(Power(y, n - 1),
                           diff2(
                             Times(tl), x, count + n, sumcount,
                             Power(y, n) :: ls))) 
                      } else {
                        Times(
                          List(Power(y, n),
                           diff2(
                             Times(tl), x, count + n, sumcount,
                             Power(y, n) :: ls)))
                      } 
                  } else {
                    Times(
                      List(Power(y, n),
                       diff2(Times(tl), x, count, sumcount, Power(y, n) :: ls)))
                  }
              }
              case Sum(lst2) => {
                
                  if (
                    count == 0
                  ) {
                    Sum(
                      List(Times(
                         List(diff2(
                            Times(ls ++ tl), x, count, sumcount,
                            Sum(lst2) :: ls),
                          Sum(lst2))),
                       Times(
                         List(Times(Const(1) :: (ls ++ tl)),
                          diff2(
                            Sum(lst2), x, count, sumcount, Sum(lst2) :: ls))))) 
                  } else {
                    Sum(
                      List(Times(
                         List(diff2(
                            Times(ls ++ tl), x, 0, sumcount, Sum(lst2) :: ls),
                          Sum(lst2))),
                       Times(
                         List(Times(Const(1) :: (ls ++ tl)),
                          diff2(
                            Sum(lst2), x, count, sumcount, Sum(lst2) :: ls)))))
                  }
              }
              case Times(lst2) => {
                Times(
                  List(diff2(Times(lst2), x, count, sumcount, ls),
                   diff2(Times(tl), x, count, sumcount, ls)))
              }
            }
          }
        }
      }
      case Sum(lst) => {
        lst match {
          case Nil() => { Sum(List(Const(0))) }
          case Cons(hd, tl) => {
            hd match {
              case Const(n) => {
                Sum(List(Const(0), diff2(Sum(tl), x, count, sumcount, ls)))
              }
              case Var(y) => {
                
                  if (
                    y == x
                  ) {
                    
                      if (
                        sumcount == 0
                      ) {
                        Sum(List(Const(1), diff2(Sum(tl), x, count, 1, ls))) 
                      } else {
                        Sum(
                          List(Const(1),
                           diff2(Sum(tl), x, count, sumcount, ls)))
                      } 
                  } else {
                    Sum(List(Const(0), diff2(Sum(tl), x, count, sumcount, ls)))
                  }
              }
              case Power(y, n) => {
                
                  if (
                    y == x
                  ) {
                    
                      if (
                        sumcount == 0
                      ) {
                        Sum(
                          List(Times(List(Const(n), Power(y, n - 1))),
                           diff2(Sum(tl), x, count, 1, ls))) 
                      } else {
                        Sum(
                          List(Times(List(Const(n), Power(y, n - 1))),
                           diff2(Sum(tl), x, count, sumcount, ls)))
                      } 
                  } else {
                    Sum(List(Const(0), diff2(Sum(tl), x, count, sumcount, ls)))
                  }
              }
              case Sum(lst2) => {
                Sum(
                  List(diff2(Sum(lst2), x, count, sumcount, ls),
                   diff2(Sum(tl), x, count, sumcount, ls)))
              }
              case Times(lst2) => {
                Sum(
                  List(diff2(Times(lst2), x, count, sumcount, Nil()),
                   diff2(Sum(tl), x, count, sumcount, ls)))
              }
            }
          }
        }
      }
      case Const(n) => { Const(0) }
      case Var(y) => { if (y == x) Const(1) else Const(0) }
      case Power(y, n) => {
        if (y == x) Times(List(Const(n), Power(y, n - 1))) else Const(0)
      }
    }
  }
  
  def diff: (Aexp, String) => Aexp = {
    case (e, x) => { diff2(e, x, 0, 0, Nil()) }
  }
}