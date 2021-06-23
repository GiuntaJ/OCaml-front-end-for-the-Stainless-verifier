import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub10 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff(((sic, v))) = {
    sic match {
      case Const(n) => { Const(0) }
      case Var(x) => { if (x == v) Const(1) else Const(0) }
      case Power(x, n) => {
        
          if (
            x == v
          ) {
            
              if (
                n == 0
              ) {
                Const(0) 
              } else if (
                n == 1
              ) {
                Const(1) 
              } else {
                Times(List(Const(n), Power(x, n - 1)))
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
              case Cons(h, Nil()) => { diff(h, v) }
              case Cons(h, t) => {
                h match {
                  case Const(n) => { Times(List(h, diff(Times(t), v))) }
                  case _ => {
                    Sum(
                      List(Times(List(diff(h, v), Times(t))),
                       Times(List(h, diff(Times(t), v)))))
                  }
                }
              }
            }
          }
      }
      case Sum(l) => {
        l match {
          case Cons(h, Nil()) => { diff(h, v) }
          case Cons(h, t) => { Sum(List(diff(h, v), diff(Sum(t), v))) }
        }
      }
    }
  }
}