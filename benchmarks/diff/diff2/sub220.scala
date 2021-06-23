import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub220 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff_check: (List[Aexp], Int63) => Int63 = {
    case (lst, count) =>
      {
        lst match {
          case Nil() => { count }
          case Cons(hd, tl) => {
            hd match {
              case Const(num) => { diff_check(tl, count) }
              case _ => { diff_check(tl, count + 1) }
            }
          }
        }
    }
  }
  
  def diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        e match {
          case Const(num) => { Const(0) }
          case Var(str) => { if (str == x) Const(1) else Var(str) }
          case Power(str, num) => {
            
              if (
                str == x
              ) {
                
                  if (
                    num <= 1
                  ) {
                    Const(num) 
                  } else {
                    Times(List(Const(num), Power(str, num - 1)))
                  } 
              } else {
                Power(str, num)
              }
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                val _2 = {
                  val cnt = diff_check(lst, 0)
                  
                    if (
                      cnt == 0
                    ) {
                      Const(0) 
                    } else {
                      hd match {
                        case Const(num) => {
                          Times(List(Const(num), diff(Times(tl), x)))
                        }
                        case _ => {
                          tl match {
                            case Nil() => { Times(List(diff(hd, x))) }
                            case _ => {
                              Times(List(diff(hd, x), diff(Times(tl), x)))
                            }
                          }
                        }
                      }
                    }
                }
              }
            }
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                hd match {
                  case _ => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
                }
              }
            }
          }
        }
    }
  }
}
