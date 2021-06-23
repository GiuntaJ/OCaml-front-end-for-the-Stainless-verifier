import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub104 {
  
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  
  def diff: (Aexp, String) => Aexp = {
    case (ae, va) =>
      {
        ae match {
          case Const(a) => { Const(0) }
          case Var(x) => { if (x == va) Const(1) else Const(0) }
          case Power(x, a) => {
            if (x == va) Times(List(Const(a), Power(x, a - 1))) else Const(0)
          }
          case Times(Nil()) => { Const(0) }
          case Times(Cons(hd, Nil())) => { diff(hd, va) }
          case Times(Cons(hd, tl)) => {
            Sum(
              List(Times(diff(hd, va) :: tl),
               Times(List(hd, diff(Times(tl), va)))))
          }
          case Sum(Nil()) => { Const(0) }
          case Sum(Cons(hd, Nil())) => { diff(hd, va) }
          case Sum(Cons(hd, tl)) => { Sum(List(diff(hd, va), diff(Sum(tl), va)))
          }
        }
    }
  } 
}