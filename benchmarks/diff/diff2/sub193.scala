import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub193 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        e match {
          case Const(i) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(a, i) => {
            
              if (
                a == x
              ) {
                i match {
                  case 1 => { Const(i) }
                  case 2 => { Times(List(Const(i), Var(a))) }
                  case _ => { Times(List(Const(i), Power(a, i - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Times(lst) => {
            
              if (
                (val _9 = {
                   def search(lst, x) = {
                     lst match {
                       case Cons(hd, tl) => {
                         hd match {
                           case Power(a, i) => { 1 + search(tl, x) }
                           case Var(a) => { if (a == x) 1 else 0 + search(tl, x)
                           }
                           case _ => { search(tl, x) }
                         }
                       }
                       case _ => { 0 }
                     }
                   }
                   search(lst, x)
                 }) ==
                  0
              ) {
                Const(0) 
              } else {
                Times(
                  val _12 = {
                    def yes(lst, x) = {
                      lst match {
                        case Cons(hd, tl) => {
                          hd match {
                            case Const(i) => { Const(i) :: yes(tl, x) }
                            case Var(a) => {
                              (if (a == x) diff(hd, x) else hd) :: yes(tl, x)
                            }
                            case Power(a, i) => {
                              (if (a == x) diff(hd, x) else hd) :: yes(tl, x)
                            }
                            case _ => { diff(hd, x) :: yes(tl, x) }
                          }
                        }
                        case _ => { Nil() }
                      }
                    }
                    yes(lst, x)
                  })
              }
          }
          case Sum(lst) => {
            Sum(
              val _4 = {
                def sumdiff(lst, x) = {
                  lst match {
                    case Cons(hd, tl) => { diff(hd, x) :: sumdiff(tl, x) }
                    case Nil() => { Nil() }
                  }
                }
                sumdiff(lst, x)
              })
          }
        }
    }
  }
}