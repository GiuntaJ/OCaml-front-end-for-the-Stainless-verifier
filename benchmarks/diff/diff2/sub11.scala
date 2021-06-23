import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub11 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def find_ch: (List[Aexp], String) => Boolean = {
    case (l, x) =>
      {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => {
            hd match {
              case Var(str) => { if (str == x) true else find_ch(tl, x) }
              case Power(str, _) => { if (str == x) true else find_ch(tl, x) }
              case _ => { find_ch(tl, x) }
            }
          }
        }
    }
  }
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, Nil()) => { diff(hd, x) }
              case Cons(hd, tl) => {
                Sum(List(diff(hd, x)) ++ List(diff(Sum(tl), x)))
              }
            }
          }
          case Times(l) => {
            
              if (
                find_ch(l, x)
              ) {
                l match {
                  case Nil() => { Const(0) }
                  case Cons(hd, tl) => {
                    hd match {
                      case Const(n) => {
                        Times(List(Const(n)) ++ List(diff(Times(tl), x)))
                      }
                      case Var(v) => {
                        
                          if (
                            v == x
                          ) {
                            tl match {
                              case Nil() => { diff(Var(v), x) }
                              case _ => { diff(Times(tl ++ List(Var(v))), x) }
                            } 
                          } else {
                            Times(List(Var(v)) ++ List(diff(Times(tl), x)))
                          }
                      }
                      case Power(v, n) => {
                        
                          if (
                            v == x
                          ) {
                            tl match {
                              case Nil() => { diff(Power(v, n), x) }
                              case _ => {
                                diff(Times(tl ++ List(Power(v, n))), x)
                              }
                            } 
                          } else {
                            Times(List(Power(v, n)) ++ List(diff(Times(tl), x)))
                          }
                      }
                      case _ => { Times(l) }
                    }
                  }
                } 
              } else {
                Const(0)
              }
          }
          case Var(v) => { if (v == x) Const(1) else Const(0) }
          case Power(v, n) => {
            
              if (
                v == x
              ) {
                n match {
                  case 1 => { Const(1) }
                  case 2 => { Times(List(Const(2)) ++ List(Var(v))) }
                  case _ => { Times(List(Const(n)) ++ List(Power(v, n - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Const(n) => { Const(0) }
        }
    }
  }
}