import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub290 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(c) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(a, n) => {
            
              if (
                a == x
              ) {
                Times(List(Const(n)) ++ List(Power(a, n - 1))) 
              } else {
                Const(0)
              }
          }
          case Times(Nil()) => { Const(0) }
          case Times(Cons(hd, tl)) => {
            Sum(
              List(Times(List(diff(hd, x)) ++ tl)) ++
              List(Times(List(hd) ++ List(diff(Times(tl), x)))))
          }
          case Sum(Nil()) => { Const(0) }
          case Sum(Cons(hd, tl)) => {
            Sum(List(diff(hd, x)) ++ List(diff(Sum(tl), x)))
          }
        }
    }
  }
  
  /*
  let test = Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1];;
  diff (test, "x");;
  */
}