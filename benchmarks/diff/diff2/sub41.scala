import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub41 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(a) => { Const(0) }
          case Var(y) => { if (x == y) Const(1) else Const(0) }
          case Power(y, a) => {
            
              if (
                x != y
              ) {
                Const(0) 
              } else if (
                a == 1
              ) {
                diff(Var(y), x) 
              } else {
                Times(List(Const(a), Power(y, a - 1)))
              }
          }
          case Times(Cons(a, b)) => { Times(List(a, diff(Sum(b), x))) }
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(h, t) => { Sum(List(diff(h, x), diff(Sum(t), x))) }
            }
          }
        }
    }
  }
}