import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub116 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(_) => { Const(0) }
          case Var(str) => { if (var0 == str) Const(1) else Const(0) }
          case Power(str, num) => {
            
              if (
                str == var0
              ) {
                num match {
                  case 0 => { Const(0) }
                  case _ => { Times(List(Const(num), Power(str, num - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Times(Nil()) => { Const(0) }
          case Times(Cons(h, t)) => {
            Sum(
              List(Times(List(h, diff(Times(t), var0))),
               Times(diff(h, var0) :: t)))
          }
          case Sum(exp) => { Sum(exp.map(( (x) => { diff(x, var0) } ))) }
        }
    }
  }
}