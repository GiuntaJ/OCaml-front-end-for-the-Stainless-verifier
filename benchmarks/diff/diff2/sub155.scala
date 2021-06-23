import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub155 {
  
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
          case Var(k) => { if (var0 == k) Const(1) else Const(0) }
          case Power(k, m) => {
            
              if (
                not(var0 == k)
              ) {
                Const(0) 
              } else {
                Times(List(Const(m), Power(k, m - 1)))
              }
          }
          case Times(Nil()) => { Const(0) }
          case Times(Cons(head, tail)) => {
            Sum(
              List(Times(diff(head, var0) :: tail),
               Times(List(head, diff(Times(tail), var0)))))
          }
          case Sum(aexps) => { Sum(aexps.map(( (dum) => { diff(dum, var0) } )))
          }
        }
    }
  }
}