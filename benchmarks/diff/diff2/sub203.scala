import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub203 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def simplifier(f: Aexp): Aexp = {
    val _2 = {
      val res = f match {
        case Times(l) => {
          
            if (
              l.exists(( (x) => { if (x == Const(0)) true else false } ))
            ) {
              Const(0) 
            } else {
              l match {
                case Nil() => { Times(Nil()) }
                case Cons(hd, tl) => {
                  val _9 = {
                    val lst = simplifier(Times(tl)) match {
                      case Times(lst) => { lst }
                      case _ => { Nil() }
                    }
                    Times(simplifier(hd) :: lst)
                  }
                }
              }
            }
        }
        case Sum(l) => {
          l match {
            case Nil() => { Sum(Nil()) }
            case Cons(hd, tl) => {
              
                if (
                  hd == Const(0)
                ) {
                  Sum(tl) 
                } else {
                  val _6 = {
                    val lst = simplifier(Sum(tl)) match {
                      case Sum(lst) => { lst }
                      case _ => { Nil() }
                    }
                    Sum(simplifier(hd) :: lst)
                  }
                }
            }
          }
        }
        case Power(s, n) => {
          n match {
            case 0 => { Const(1) }
            case 1 => { Var(s) }
            case _ => { f }
          }
        }
        case _ => { f }
      }
      if (f == res) res else simplifier(res)
    }
  }
  
  
  def diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        val _12 = {
          val res = e match {
            case Const(n) => { Const(0) }
            case Var(s) => { if (s == x) Const(1) else Const(0) }
            case Times(l) => {
              l match {
                case Nil() => { Const(0) }
                case Cons(hd, tl) => {
                  Sum(
                    List(Times(diff(hd, x) :: tl),
                     Times(List(hd, diff(Times(tl), x)))))
                }
              }
            }
            case Sum(l) => {
              l match {
                case Nil() => { Const(0) }
                case Cons(y, Nil()) => { diff(y, x) }
                case Cons(hd, tl) => {
                  val _15 = {
                    val l1 = List(diff(hd, x))
                    val _16 = {
                      val l2 = diff(Sum(tl), x) match {
                        case Sum(l) => { l1 ++ l }
                        case i => { l1 ++ List(i) }
                      }
                      Sum(l2)
                    }
                  }
                }
              }
            }
            case Power(s, n) => {
              (s, n) match {
                case (_, 0) => { Const(0) }
                case (s, 1) => { diff(Var(s), x) }
                case (p, q) => {
                  
                    if (
                      p == x
                    ) {
                      Times(List(Const(q), Power(p, q - 1))) 
                    } else {
                      Const(0)
                    }
                }
              }
            }
          }
          simplifier(res)
        }
    }
  }
}