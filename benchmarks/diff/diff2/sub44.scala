import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub44 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        val _2 = {
          def diffList: List[Aexp] => List[Aexp] = (
            (al) =>
              {
                al match {
                  case Nil() => { Nil() }
                  case Cons(e, Nil()) => { List(diff(e, x)) }
                  case Cons(hd, tl) => { diff(hd, x) :: diffList(tl) }
                }
            }
          )
          aexp match {
            case Const(c) => { Const(0) }
            case Var(v) => { Var(v) }
            case Power(v, c) => {
              if (v == x) Times(List(Const(c), Power(v, c - 1))) else Const(0)
            }
            case Times(l) => {
              l match {
                case Nil() => { Times(Nil()) }
                case Cons(hd, tl) => { Times(diffList(l)) }
              }
            }
            case Sum(l) => {
              l match {
                case Nil() => { Sum(Nil()) }
                case Cons(hd, tl) => { Sum(diffList(l)) }
              }
            }
          }
        }
    }
  }
}