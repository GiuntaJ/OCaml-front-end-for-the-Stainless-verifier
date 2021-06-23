import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub117 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(x) => { if (x == var0) Const(1) else Const(0) }
          case Power(x, n) => {
            
              if (
                x == var0
              ) {
                
                  if (
                    n == 1
                  ) {
                    Const(n) 
                  } else {
                    Times(List(Const(n), Power(x, n - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(lst) => {
            val _5 = {
              def length(l) = {
                l match {
                  case Nil() => { 0 }
                  case Cons(hd, tl) => { 1 + length(tl) }
                }
              }
              val _6 = {
                def check1: (List[Aexp], Int63) => List[Aexp] = {
                  case (lst_0, n) =>
                    {
                      lst_0 match {
                        case Nil() => { Nil() }
                        case Cons(hd, tl) => {
                          
                            if (
                              n == 0
                            ) {
                              List(diff(hd, var0)) ++ tl 
                            } else {
                              hd :: check1(tl, n - 1)
                            }
                        }
                      }
                  }
                }
                val _7 = {
                  def check2: (List[Aexp], Int63) => List[Aexp] = {
                    case (lst_0, n) =>
                      {
                        
                          if (
                            n == length(lst_0) - 1
                          ) {
                            List(Times(check1(lst_0, n))) 
                          } else {
                            List(Times(check1(lst_0, n))) ++
                            check2(lst_0, n + 1)
                          }
                    }
                  }
                  Sum(check2(lst, 0))
                }
              }
            }
          }
          case Sum(lst) => {
            val _2 = {
              def check: List[Aexp] => List[Aexp] = (
                (lst_0) =>
                  {
                    lst_0 match {
                      case Nil() => { Nil() }
                      case Cons(hd, Nil()) => { List(diff(hd, var0)) }
                      case Cons(hd, tl) => { List(diff(hd, var0)) ++ check(tl) }
                    }
                }
              )
              Sum(check(lst))
            }
          }
        }
    }
  }
}