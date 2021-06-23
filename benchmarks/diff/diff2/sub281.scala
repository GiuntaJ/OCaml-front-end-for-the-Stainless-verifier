import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub281 {
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
          case Const(a) => { Const(0) }
          case Var(myvar) => { if (myvar == x) Const(1) else Var(myvar) }
          case Power(myvar, i) => {
            
              if (
                myvar == x
              ) {
                Times(List(Const(i), Power(myvar, i - 1))) 
              } else {
                Power(myvar, i)
              }
          }
          case Times(Cons(hd, tl)) => {
            Sum(List(Times(List(diff(hd, x), Sum(tl))), diff(Sum(tl), x)))
          }
          case Times(Nil()) => { Const(0) }
          case Sum(Cons(hd, tl)) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
          case Sum(Nil()) => { Const(0) }
        }
    }
  }
    /*
    | Sum explst ->
      match explst with
        | [] -> Const 0 //not exhaustive error 안뜨게끔 추가
        | hd::tl -> Sum[ diff(hd,x) ; diff(Sum tl,x) ];; 
    */
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
  diff(Sum(List(Power("x", 2))), "x")
  diff(Var("x"), "x")
  diff(Const(1), "x")
  diff(Sum(List(Var("x"), Var("x"))), "x")
  diff(Sum(List(Power("x", 2), Const(2))), "x")
}