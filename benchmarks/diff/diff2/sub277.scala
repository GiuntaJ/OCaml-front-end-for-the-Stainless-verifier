import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub277 {
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
          def fmap(f, lst) = {
            lst match {
              case Nil() => { Nil() }
              case Cons(x, xs) => { f(x) :: fmap(f, xs) }
            }
          }
          val _3 = {
            def diff_0(var0, expr) = {
              expr match {
                case Sum(lst) => { Sum(fmap(diff_0(var0), lst)) }
                case Times(lst) => {
                  lst match {
                    case Nil() => { Const(0) }
                    case Cons(x, xs) => {
                      Sum(
                        List(Times(List(diff_0(var0, x), Times(xs))),
                         Times(List(x, Times(fmap(diff_0(var0), xs))))))
                    }
                  }
                }
                case Power(v, e) => {
                  
                    if (
                      v == var0
                    ) {
                      Times(List(Const(e), Power(v, e - 1))) 
                    } else {
                      Const(0)
                    }
                }
                case Var(v) => { if (v == var0) Const(1) else Const(0) }
                case Const(i) => { Const(0) }
              }
            }
            diff_0(x, exp)
          }
        }
    }
  }
    
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}