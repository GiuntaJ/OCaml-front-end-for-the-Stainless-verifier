import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub110 {
  
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
          case Const(a) => { Const(a) }
          case Var(x) => { Var(x) }
          case Power(x, n) => {
            
              if (
                x == var0
              ) {
                Times(List(Const(n), Power(x, n - 1))) 
              } else {
                Power(x, n)
              }
          }
          case Times(l) => {
            l match {
              case Nil() => { assert(false, "Failure with error") }
              case Cons(h, t) => {
                Sum(List(Times(diff(h, var0) :: t), diff(h, var0)))
              }
            }
          }
          case Sum(m) => {
            m match {
              case Nil() => { assert(false, "Failure with error") }
              case Cons(h, t) => { Sum(List(diff(h, var0), diff(h, var0))) }
            }
          }
        }
    }
  }
}