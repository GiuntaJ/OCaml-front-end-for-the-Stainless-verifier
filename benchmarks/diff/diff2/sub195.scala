import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub195 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  val diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        val _2 = {
          def subdiff(e, x) = {
            e match {
              case Const(_) => { Const(0) }
              case Var(s) => { if (s == x) Const(1) else Const(0) }
              case Power(s, n) => {
                if (s == x) Times(List(Const(n), Power(s, n - 1))) else Const(0)
              }
              case Times(al) => {
                val _8 = {
                  def sub(b) = {
                    b match {
                      case Nil() => { Const(0) }
                      case Cons(hd, tl) => {
                        Sum(
                          List(Times(subdiff(hd, x) :: tl),
                           Times(List(hd, sub(tl)))))
                      }
                    }
                  }
                  sub(al)
                }
              }
              case Sum(al) => {
                val _5 = {
                  def sub(b) = {
                    b match {
                      case Nil() => { Nil() }
                      case Cons(hd, tl) => { subdiff(hd, x) :: sub(tl) }
                    }
                  }
                  Sum(sub(al))
                }
              }
            }
          }
          subdiff(e, x)
        }
    }
  }
}