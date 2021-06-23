import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub287 {
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
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(a, b) => {
            if (a == x) Times(List(Const(b), Power(a, b - 1))) else Const(0)
          }
          case Sum(lst) => {
            val _6 = {
              def addition: (List[Aexp], List[Aexp]) => Aexp = {
                case (l, result) =>
                  {
                    l match {
                      case Nil() => { Sum(result) }
                      case Cons(hd, tl) => {
                        addition(tl, result ++ List(diff(hd, x)))
                      }
                    }
                }
              }
              addition(lst, Nil())
            }
          }
          case Times(lst) => {
            val _2 = {
              val copy_lst = lst
              val _3 = {
                def multiple: (Int63, List[Aexp], List[Aexp]) => Aexp = {
                  case (flag, l, result) =>
                    {
                      l match {
                        case Nil() => { Sum(result) }
                        case Cons(hd, tl) => {
                          
                            if (
                              copy_lst == l
                            ) {
                              
                                if (
                                  flag == 0
                                ) {
                                  multiple(
                                    1, tl ++ List(hd),
                                    result ++
                                    List(Times(List(diff(hd, x)) ++ tl))) 
                                } else {
                                  Sum(result)
                                } 
                            } else {
                              multiple(
                                1, tl ++ List(hd),
                                result ++ List(Times(List(diff(hd, x)) ++ tl)))
                            }
                        }
                      }
                  }
                }
                multiple(0, lst, Nil())
              }
            }
          }
          case Const(a) => { Const(0) }
        }
    }
  }
        
}