import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub215 {
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
          case Const(i) => { Const(0) }
          case Var(s) => { if (s == x) Const(1) else Const(0) }
          case Power(s, i) => {
            if (s == x) Times(List(Const(i), Power(s, i - 1))) else Const(0)
          }
          case Times(l) => {
            val _5 = {
              def difft: List[Aexp] => List[Aexp] = (
                (l) =>
                  {
                    l match {
                      case Nil() => { Nil() }
                      case Cons(hd, tl) => {
                        
                          if (
                            diff(hd, x) == Const(0)
                          ) {
                            hd :: difft(tl) 
                          } else {
                            diff(hd, x) :: difft(tl)
                          }
                      }
                    }
                }
              )
              Times(difft(l))
            }
          }
          case Sum(l) => {
            val _2 = {
              def diffs: List[Aexp] => List[Aexp] = (
                (l) =>
                  {
                    l match {
                      case Nil() => { Nil() }
                      case Cons(hd, tl) => { diff(hd, x) :: diffs(tl) }
                    }
                }
              )
              Sum(diffs(l))
            }
          }
        }
    }
  }
}