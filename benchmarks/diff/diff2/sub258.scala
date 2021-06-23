import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub258 {
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
          def times_diff_2(l, o, r) = {
            o match {
              case Nil() => { r }
              case Cons(hd, tl) => { if (hd == l) diff(hd, x) :: r else hd :: r
              }
            }
          }
          val _3 = {
            def times_diff_1(l, o, r) = {
              l match {
                case Nil() => { r }
                case Cons(hd, tl) => {
                  times_diff_1(tl, o, Times(times_diff_2(hd, o, Nil())) :: r)
                }
              }
            }
            val _4 = {
              def sum_diff(l, r) = {
                l match {
                  case Nil() => { r }
                  case Cons(hd, tl) => { sum_diff(tl, diff(hd, x) :: r) }
                }
              }
              exp match {
                case Const(s) => { Const(0) }
                case Var(s) => { if (s == x) Const(1) else Const(0) }
                case Power(s, i) => {
                  
                    if (
                      s == x
                    ) {
                      
                        if (
                          i != 0
                        ) {
                          Times(List(Const(i), Power(s, i - 1))) 
                        } else {
                          Const(0)
                        } 
                    } else {
                      Const(0)
                    }
                }
                case Times(l) => { Sum(times_diff_1(l, l, Nil())) }
                case Sum(l) => { Sum(sum_diff(l, Nil())) }
              }
            }
          }
        }
    }
  }
    
  diff(Sum(List(Power("x", 2), Times(List(Power("y", 2), Var("x"))))), "x")
}