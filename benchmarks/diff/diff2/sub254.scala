import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub254 {
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
          case Const(n) => { Const(0) }
          case Var(str) => { if (str == x) Const(1) else Var(str) }
          case Power(str, n) => {
            if (str == x) Times(List(Const(n), Power(str, n - 1))) else Const(0)
          }
          case Times(lst) => {
            val _5 = {
              def timedf: (List[Aexp], List[Aexp]) => List[Aexp] = {
                case (lst, ans) =>
                  {
                    lst match {
                      case Nil() => { ans }
                      case Cons(hd, tl) => {
                        val _8 = {
                          def isvar: List[Aexp] => Boolean = (
                            (lst) =>
                              {
                                lst match {
                                  case Nil() => { false }
                                  case Cons(hd, tl) => {
                                    hd match {
                                      case Times(lst) => {
                                        
                                          if (
                                            isvar(lst) == true
                                          ) {
                                            true 
                                          } else {
                                            isvar(tl)
                                          }
                                      }
                                      case Sum(lst) => {
                                        
                                          if (
                                            isvar(lst) == true
                                          ) {
                                            true 
                                          } else {
                                            isvar(tl)
                                          }
                                      }
                                      case Const(n) => { isvar(tl) }
                                      case Var(str) => { true }
                                      case Power(str, n) => {
                                        if (str == x) true else false
                                      }
                                    }
                                  }
                                }
                            }
                          )
                          
                            if (
                              isvar(lst) == true
                            ) {
                              hd match {
                                case Const(n) => {
                                  timedf(tl, ans ++ List(Const(n)))
                                }
                                case _ => { timedf(tl, ans ++ List(diff(hd, x)))
                                }
                              } 
                            } else {
                              ans ++ List(Const(0))
                            }
                        }
                      }
                    }
                }
              }
              Times(timedf(lst, Nil()))
            }
          }
          case Sum(lst) => {
            val _2 = {
              def sumdf: (List[Aexp], List[Aexp]) => List[Aexp] = {
                case (lst, ans) =>
                  {
                    lst match {
                      case Nil() => { ans }
                      case Cons(hd, tl) => { sumdf(tl, ans) ++ List(diff(hd, x))
                      }
                    }
                }
              }
              Sum(sumdf(lst, Nil()))
            }
          }
        }
    }
  }
      
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
  diff(
    Sum(
      List(Times(List(Const(5), Power("x", 3))),
       Times(List(Const(2), Var("x"))), Const(3))),
    "x")
  diff(Times(List(Const(5), Power("x", 3))), "x")
}