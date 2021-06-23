import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub6 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def exist: (List[Aexp], String) => Int63 = {
    case (l, x) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(hd, tl) => {
            hd match {
              case Var(v) => { if (v == x) 1 + exist(tl, x) else exist(tl, x) }
              case Power(str, i) => {
                if (str == x) 1 + exist(tl, x) else exist(tl, x)
              }
              case Times(lst) => { exist(lst, x) + exist(tl, x) }
              case Sum(lst) => { exist(lst, x) + exist(tl, x) }
              case _ => { 0 + exist(tl, x) }
            }
          }
        }
    }
  } 
  
  def modify: (List[Aexp], String) => List[Aexp] = {
    case (l, x) =>
      {
        l match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => {
            hd match {
              case Const(n) => {
                Times(List(Power(x, 0), Const(n))) :: modify(tl, x)
              }
              case _ => { hd :: modify(tl, x) }
            }
          }
        }
    }
  }
  
  val normalized: (Aexp, String) => Aexp = {
    case (l, x) =>
      {
        val _2 = {
          val modified = modify(List(l), x)
          modified match {
            case Nil() => { assert(false, "Failure with error") }
            case Cons(hd, tl) => { hd }
          }
        }
    }
  }
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        
          if (
            exist(List(aexp), x) > 0
          ) {
            aexp match {
              case Const(n) => { Const(0) }
              case Var(str) => { Const(1) }
              case Power(str, n) => { Times(List(Const(n), Power(str, n - 1))) }
              case Times(l) => {
                l match {
                  case Nil() => { Const(1) }
                  case Cons(hd, tl) => {
                    hd match {
                      case Const(n) => {
                        Times(List(Const(n), diff(Times(tl), x)))
                      }
                      case Power(str1, n1) => {
                        
                          if (
                            n1 == 0
                          ) {
                            Times(List(Const(0), diff(Times(tl), x))) 
                          } else {
                            Times(List(diff(hd, x), diff(Times(tl), x)))
                          }
                      }
                      case _ => { Times(List(diff(hd, x), diff(Times(tl), x))) }
                    }
                  }
                }
              }
              case Sum(l) => {
                val _5 = {
                  val mod_exp = normalized(aexp, x)
                  mod_exp match {
                    case Sum(l1) => {
                      l1 match {
                        case Nil() => { Const(0) }
                        case Cons(hd, tl) => {
                          Sum(List(diff(hd, x), diff(Sum(tl), x)))
                        }
                      }
                    }
                    case _ => { assert(false, "Failure with error") }
                  }
                }
              }
            } 
          } else {
            aexp match {
              case Times(Nil()) => { Const(1) }
              case _ => { Const(0) }
            }
          }
    }
  }
}