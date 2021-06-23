import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub96 {
  
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
          case Const(x) => { Const(0) }
          case Var(x) => { if (x == var0) Const(1) else Const(0) }
          case Power(x, y) => {
            if (x == var0) Times(List(Const(y), Power(x, y - 1))) else Const(0)
          }
          case Times(x) => { Const(0) }
          case Sum(x) => {
            val _2 = {
              def diff1: List[Aexp] => List[Aexp] = (
                (l) =>
                  {
                    l match {
                      case Nil() => { Nil() }
                      case Cons(hd, tl) => { diff(hd, var0) :: diff1(tl) }
                    }
                }
              )
              Sum(diff1(x))
            }
          }
        }
    }
  } 
}