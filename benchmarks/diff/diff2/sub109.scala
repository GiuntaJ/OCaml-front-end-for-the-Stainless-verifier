import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub109 {
  
   sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def hasVar(((exp, var0))) = {
    exp match {
      case Cons(hd, tl) => {
        (hd match {
           case Const(x) => { false }
           case Var(x) => { if (x == var0) true else false }
           case Power(x, y) => {
             if (x == var0) if (y ne 0) true else false else false
           }
           case Times(l) => { hasVar(l, var0) }
           case Sum(l) => { hasVar(l, var0) }
         }) ||
        hasVar(tl, var0)
      }
      case Nil() => { false }
    }
  }
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Power(x, y) => {
            
              if (
                x == var0
              ) {
                
                  if (
                    y == 0
                  ) {
                    Const(0) 
                  } else if (
                    y == 1
                  ) {
                    Const(y) 
                  } else {
                    Times(List(Const(y), Power(x, y - 1)))
                  } 
              } else if (
                y == 0
              ) {
                Var(x) 
              } else {
                Power(x, y)
              }
          }
          case Const(x) => { Const(0) }
          case Var(x) => { if (x == var0) Const(1) else Const(0) }
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(x, Nil()) => { diff(x, var0) }
              case Cons(hd, tl) => {
                Sum(List(diff(hd, var0), diff(Sum(tl), var0)))
              }
            }
          }
          case Times(l) => {
            val _2 = {
              val flag = hasVar(l, var0)
              l match {
                case Nil() => { Const(0) }
                case Cons(x, Nil()) => {
                  
                    if (
                      flag == true
                    ) {
                      if (diff(x, var0) == Const(0)) x else diff(x, var0) 
                    } else {
                      Const(0)
                    }
                }
                case Cons(hd, tl) => {
                  Times(
                    List(
                       if (
                         flag == true
                       ) {
                         if (diff(hd, var0) == Const(0)) hd else diff(hd, var0) 
                       } else {
                         Const(0)
                       },
                     diff(Times(tl), var0)))
                }
              }
            }
          }
        }
    }
  }
}