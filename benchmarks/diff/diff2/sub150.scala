import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub150 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
    
    def map(f, l) = {
    l match {
      case Nil() => { Nil() }
      case Cons(h, t) => { f(h) :: map(f, t) }
    }
  }
    
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(x) => { Const(0) }
          case Var(x) => { if (x == var0) Const(1) else Const(0) }
          case Power(str, n) => {
            
              if (
                str == var0
              ) {
                Times(List(Const(n), Power(str, n - 1))) 
              } else {
                Const(0)
              }
          }
          case Times(Nil()) => { Const(0) }
          case Times(Cons(h, t)) => {
            Sum(
              List(Times(diff(h, var0) :: t),
               Times(List(h, diff(Times(t), var0)))))
          }
          case Sum(x) => { Sum(map(( (k) => { diff(k, var0) } ), x)) }
        }
    }
  }
    
}