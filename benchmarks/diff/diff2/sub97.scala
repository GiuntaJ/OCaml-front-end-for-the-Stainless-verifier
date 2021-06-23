import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub97 {
  
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
          case Const(sth) => { Const(0) }
          case Var(sth) => { if (sth == var0) Const(1) else Const(0) }
          case Power(sth, n) => {
            
              if (
                sth != var0
              ) {
                Const(0) 
              } else {
                Times(List(Const(n), Power(sth, n - 1)))
              }
          }
          case Times(sth) => {
            sth match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(diff(hd, var0) :: tl),
                   Times(List(hd, diff(Times(tl), var0)))))
              }
            }
          }
          case Sum(sth) => {
            val _2 = {
              def map(f, l) = {
                l match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => { f(hd) :: map(f, tl) }
                }
              }
              sth match {
                case Nil() => { Const(0) }
                case _ => {
                  Sum(map(( (element) => { diff(element, var0) } ), sth))
                }
              }
            }
          }
        }
    }
  }  
}