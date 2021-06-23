import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub107 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, env) =>
      {
        exp match {
          case Const(a) => { Const(0) }
          case Var(x) => { if (x == env) Const(1) else Var(x) }
          case Power(s, i) => {
            
              if (
                s == env
              ) {
                Times(List(Const(i), Power(s, i - 1))) 
              } else {
                Power(s, i)
              }
          }
        }
    }
  }
}