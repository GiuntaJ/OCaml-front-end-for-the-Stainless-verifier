import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub60 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff(((aexp, x))) = {
    aexp match {
      case Const(int1) => { Const(0) }
      case Var(x) => { Const(1) }
      case Power(var0, int1) => {
        Times(List(Const(int1), Power(var0, int1 - 1)))
      }
      case Times(lst) => {
        lst match {
          case Nil() => { Const(1) }
          case Cons(hd, tail) => {
            
              if (
                diff(hd, x) == Const(0)
              ) {
                Times(List(hd, diff(Times(tail), x))) 
              } else if (
                diff(hd, x) == Const(1)
              ) {
                Times(Const(1) :: Const(1) :: tail) 
              } else {
                Times(Const(1) :: diff(hd, x) :: tail)
              }
          }
        }
      }
      case Sum(lst) => {
        lst match {
          case Nil() => { Const(0) }
          case Cons(hd, tail) => { Sum(List(diff(hd, x), diff(Sum(tail), x))) }
        }
      }
    }
  }
}