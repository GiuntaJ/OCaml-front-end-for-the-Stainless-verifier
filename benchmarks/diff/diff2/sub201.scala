import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub201 {
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
          case Var(y) => { if (y == x) Const(1) else Const(0) }
          case Power(y, i) => {
            if (y == x) Times(List(Const(i), Power(y, i - 1))) else Const(0)
          }
          case Times(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(single_e, Nil()) => { diff(single_e, x) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(diff(hd, x) :: tl),
                   Times(List(hd, diff(Times(tl), x)))))
              }
            }
          }
          case Sum(l) => {
            val _2 = {
              val derived_list = List.fold_left(
                {
                  case (acc, e_prime) => { acc ++ List(diff(e_prime, x)) }
                },
                Nil(), l)
              Sum(derived_list)
            }
          }
        }
    }
  }
}