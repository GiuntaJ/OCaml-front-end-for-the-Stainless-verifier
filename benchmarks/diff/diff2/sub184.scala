import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub184 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        e match {
          case Const(a) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Var(a) }
          case Power(a, b) => {
            
              if (
                a == x && b == 1
              ) {
                Const(1) 
              } else if (
                a == x
              ) {
                Times(List(Const(b), Power(a, b - 1))) 
              } else {
                Power(a, b)
              }
          }
          case Times(a) => {
            a match {
              case Cons(n1, Cons(n2, Nil())) => {
                
                  if (
                    n1 == Const(0) || n2 == Const(0)
                  ) {
                    Const(0) 
                  } else if (
                    n1 == Var(x)
                  ) {
                    n2 
                  } else if (
                    n2 == Var(x)
                  ) {
                    n1 
                  } else {
                    Times(List(n1, diff(n2, x)))
                  }
              }
            }
          }
          case Sum(a) => {
            val _2 = {
              def help_diff(e1) = {
                e1 match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => { diff(hd, x) :: help_diff(tl) }
                }
              }
              Sum(help_diff(a))
            }
          }
        }
    }
  }
}