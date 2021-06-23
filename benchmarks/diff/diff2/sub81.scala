import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub81 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def map(f, l) = {
    l match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => { f(hd) :: map(f, tl) }
    }
  }
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(str) => { if (str == var0) Const(1) else Const(0) }
          case Power(_, 0) => { Const(0) }
          case Power(str, n) => {
            
              if (
                str != var0
              ) {
                Const(0) 
              } else {
                Times(List(Const(n), Power(str, n - 1)))
              }
          }
          case Times(Nil()) => { Const(0) }
          case Times(Cons(hd, tl)) => {
            Sum(
              List(Times(diff(hd, var0) :: tl),
               Times(List(hd, diff(Times(tl), var0)))))
          }
          case Sum(Nil()) => { Const(0) }
          case Sum(l) => { Sum(map(( (exp) => { diff(exp, var0) } ), l)) }
        }
    }
  }
}