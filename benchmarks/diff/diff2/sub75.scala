import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub75 {
  
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
          case Const(c) => { Const(0) }
          case Var(v) => { if (v == var0) Const(1) else Const(0) }
          case Power(st, i) => {
            
              if (
                st == var0
              ) {
                Times(List(Const(i), Power(st, i - 1))) 
              } else {
                Power(st, i)
              }
          }
          case Times(Cons(h, t)) => { h }
          case Sum(Cons(h, t)) => {
            val _2 = {
              def diffHelp(alexp) = { diff(alexp, var0) }
              Sum(diff(h, var0) :: t.map(diffHelp))
            }
          }
          case _ => { exp }
        }
    }
  }
}