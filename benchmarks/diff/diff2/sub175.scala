import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub175 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  val diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        val _2 = {
          def helper(((e, x))) = {
            e match {
              case Const(a) => { Const(0) }
              case Var(v) => { if (v == x) Const(1) else Const(0) }
              case Power(s, i) => {
                if (s == x) Times(List(Const(i), Power(s, i - 1))) else Const(0)
              }
            }
          }
          helper(e, x)
        }
    }
  }
}