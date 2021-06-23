import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub280 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
    
  def num_x: (List[Aexp], String) => Int63 = {
    case (lst, s) =>
      {
        
          if (
            lst == Nil()
          ) {
            0 
          } else {
            lst.head match {
              case Times(a_0) => { num_x(a_0, s) + num_x(lst.tail, s) }
              case Power(a_0, b_0) => {
                
                  if (
                    a_0 == s && b_0 != 0
                  ) {
                    b_0 + num_x(lst.tail, s) 
                  } else {
                    0 + num_x(lst.tail, s)
                  }
              }
              case Var(a_0) => {
                if (a_0 == s) 1 + num_x(lst.tail, s) else 0 + num_x(lst.tail, s)
              }
              case Const(a_0) => { if (a_0 == 0) 0 else 0 + num_x(lst.tail, s) }
            }
          }
    }
  }
        
  def dif_times_lst: (List[Aexp], String) => List[Aexp] = {
    case (lst, x) =>
      {
        lst.head match {
          case Const(x_0) => { List(Const(x_0)) ++ dif_times_lst(lst.tail, x) }
          case Var(x_0) => {
            
              if (
                x_0 == x
              ) {
                lst.tail 
              } else {
                List(Var(x_0)) ++ dif_times_lst(lst.tail, x)
              }
          }
          case _ => { Nil() }
        }
    }
  }
  
  def dif_Times: (List[Aexp], String) => List[Aexp] = {
    case (lst, x) =>
      {
        num_x(lst, x) match {
          case 0 => { List(Const(0)) }
          case 1 => { dif_times_lst(lst, x) }
          case _ => { List(Const(num_x(lst, x))) ++ dif_times_lst(lst, x) }
        }
    }
  }
    
  def dif_sums_lst: (List[Aexp], String) => List[Aexp] = {
    case (lst, x) =>
      {
        lst match {
          case Nil() => { Nil() }
          case _ => {
            lst.head match {
              case Const(x_0) => { List(Const(0)) ++ dif_sums_lst(lst.tail, x) }
              case Var(x_0) => {
                
                  if (
                    x_0 == x
                  ) {
                    List(Const(1)) ++ lst.tail 
                  } else {
                    List(Const(0)) ++ dif_sums_lst(lst.tail, x)
                  }
              }
              case Sum(a_0) => {
                
                  if (
                    num_x(a_0, x) == 0
                  ) {
                    List(Const(0)) 
                  } else {
                    dif_sums_lst(a_0, x) ++ dif_sums_lst(lst.tail, x)
                  }
              }
              case Power(a_0, b_0) => {
                
                  if (
                    a_0 == x
                  ) {
                    b_0 match {
                      case 1 => { List(Const(1)) ++ dif_sums_lst(lst.tail, x) }
                      case _ => {
                        List(Times(List(Const(b_0), Power(a_0, b_0 - 1)))) ++
                        dif_sums_lst(lst.tail, x)
                      }
                    } 
                  } else {
                    List(Const(0))
                  }
              }
            }
          }
        }
    }
  }
            
  def dif_all_lst: (List[Aexp], String) => List[Aexp] = {
    case (lst, x) =>
      {
        lst match {
          case Nil() => { Nil() }
          case _ => {
            lst.head match {
              case Const(x_0) => { List(Const(0)) ++ dif_all_lst(lst.tail, x) }
              case Var(x_0) => {
                
                  if (
                    x_0 == x
                  ) {
                    List(Const(1)) ++ lst.tail 
                  } else {
                    List(Const(0)) ++ dif_all_lst(lst.tail, x)
                  }
              }
              case Sum(a_0) => {
                
                  if (
                    num_x(a_0, x) == 0
                  ) {
                    List(Const(0)) 
                  } else {
                    dif_sums_lst(a_0, x) ++ dif_all_lst(lst.tail, x)
                  }
              }
              case Power(a_0, b_0) => {
                
                  if (
                    a_0 == x
                  ) {
                    b_0 match {
                      case 1 => { List(Const(1)) ++ dif_all_lst(lst.tail, x) }
                      case _ => {
                        List(Times(List(Const(b_0), Power(a_0, b_0 - 1)))) ++
                        dif_all_lst(lst.tail, x)
                      }
                    } 
                  } else {
                    List(Const(0))
                  }
              }
              case Times(a_0) => { List(Times(dif_Times(a_0, x))) }
            }
          }
        }
    }
  }
  
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(x_0) => { Const(0) }
          case Var(x_0) => { if (x_0 == x) Const(1) else Const(0) }
          case Sum(a_0) => { Sum(dif_all_lst(a_0, x)) }
          case Power(a_0, b_0) => {
            
              if (
                a_0 == x
              ) {
                b_0 match {
                  case 1 => { Const(1) }
                  case _ => { Times(List(Const(b_0), Power(a_0, b_0 - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Times(a_0) => { Times(dif_all_lst(a_0, x)) }
        }
    }
  }
      
  diff(Sum(List(Const(2), Times(List(Const(2), Var("x"), Var("y"))))), "x")/*2+2x^2 -> 4x*/
  diff(Power("x", 4), "x")
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")/*x^2+2x+1 -> 2x+2*/
  diff(Sum(List(Power("x", 4), Times(List(Const(2), Var("x"))), Const(1))), "x")/*x^4+2x+1 -> 4x^3+2*/
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")/*x^2+2x+1 -> 2x+2*/
}