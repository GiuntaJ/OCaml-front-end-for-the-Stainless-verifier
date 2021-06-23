import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub272 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        val _2 = {
          val tolst: Aexp => List[Aexp] = (
            (exp) =>
              {
                exp match {
                  case Times(lst) => { lst }
                  case Sum(lst) => { lst }
                  case _ => { Nil() }
                }
            }
          )
          exp match {
            case Const(n) => { Const(0) }
            case Var(chr) => { if (chr == x) Const(1) else Const(0) }
            case Power(chr, i) => {
              
                if (
                  chr == x
                ) {
                  Times(List(Const(i), Power(chr, i - 1))) 
                } else {
                  Const(0)
                }
            }
            case Sum(Cons(hd, tl)) => {
              Sum(diff(hd, x) :: tolst(diff(Sum(tl), x)))
            }
            case Times(Cons(hd, tl)) => {
              Sum(
                List(Times(List(diff(hd, x), Sum(tl))),
                 Times(List(hd, diff(Sum(tl), x)))))
            }
            case _ => { Const(0) }
          }
        }
    }
  }
      
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "y")
}