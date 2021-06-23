import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub18 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff_times(((lst, x, b))) = {
    lst match {
      case Nil() => { Const(0) }
      case Cons(hd, tl) => {
        hd match {
          case Const(a) => { diff_times(tl, x, a + b) }
          case Var(a) => { if (a == x) Const(b) else diff_times(tl, x, b) }
          case Power(a, k) => {
            
              if (
                a == x
              ) {
                
                  if (
                    k > 1
                  ) {
                    Times(List(Const(b * k), Power(a, k - 1))) 
                  } else {
                    Const(b * k)
                  } 
              } else {
                Const(0)
              }
          }
          case _ => { diff_times(tl, x, b) }
        }
      }
    }
  }
  
  def diff(((aexp, x))) = {
    aexp match {
      case Sum(lst) => {
        Sum(
          lst match {
            case Nil() => { Nil() }
            case Cons(hd, tl) => { List(diff(hd, x), diff(Sum(tl), x)) }
          })
      }
      case Times(lst) => { diff_times(lst, x, 0) }
      case Var(a) => { if (a == x) Const(1) else Const(0) }
      case Const(a) => { Const(0) }
      case Power(a, b) => {
        if (b > 1) Times(List(Const(b), Power(a, b - 1))) else Const(b)
      }
    }
  }
}