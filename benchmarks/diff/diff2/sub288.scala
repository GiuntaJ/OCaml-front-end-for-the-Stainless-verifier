import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub288 {
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
          case Const(n) => { Const(0) }
          case Var(st) => { if (st == x) Const(1) else Var(st) }
          case Power(st, n) => {
            
              if (
                st == x
              ) {
                
                  if (
                    n == 2
                  ) {
                    Times(List(Const(n), Var(st))) 
                  } else {
                    Times(List(Const(n), Power(st, n - 1)))
                  } 
              } else {
                Power(st, n)
              }
          }
          case Times(Cons(e, Nil())) => { diff(e, x) }
          case Times(Cons(hd, tl)) => {
            Sum(
              List(Times(diff(hd, x) :: tl),
               Times(List(hd, diff(Times(tl), x)))))
          }
          case Times(Nil()) => { Const(1) }
          case Sum(Cons(e, Nil())) => { diff(e, x) }
          case Sum(Cons(hd, tl)) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
          case Sum(Nil()) => { Const(0) }
        }
    }
  }
}