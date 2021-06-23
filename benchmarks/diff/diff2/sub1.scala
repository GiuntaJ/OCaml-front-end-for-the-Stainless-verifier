import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub1 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff(((sth, y))) = {
    sth match {
      case Const(x) => { Const(0) }
      case Var(x) => { if (x == y) Const(1) else Const(0) }
      case Power(x, a) => {
        
          if (
            x == y
          ) {
            
              if (
                a == 0
              ) {
                Const(0) 
              } else if (
                a == 1
              ) {
                Const(1) 
              } else if (
                a < 0
              ) {
                assert(false, "Failure with negative powered ") 
              } else {
                Times(List(Const(a), Power(x, a - 1)))
              } 
          } else {
            Const(0)
          }
      }
      case Times(l) => {
        
          if (
            l.contains(Const(0))
          ) {
            Const(0) 
          } else {
            l match {
              case Cons(hd, Nil()) => { diff(hd, y) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(List(diff(hd, y), Times(tl))),
                   Times(List(hd, diff(Times(tl), y)))))
              }
            }
          }
      }
      case Sum(l) => {
        l match {
          case Cons(hd, Nil()) => { diff(hd, y) }
          case Cons(hd, tl) => { Sum(List(diff(hd, y), diff(Sum(tl), y))) }
        }
      }
    }
  }
}
