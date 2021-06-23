import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub137 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  	def diff_sum: (List[Aexp], String) => List[Aexp] = {
    case (aexplist, var0) =>
      {
        aexplist match {
          case Nil() => { List(Const(0)) }
          case Cons(hd, tl) => { List(diff(hd, var0)) ++ diff_sum(tl, var0) }
        }
    }
  }
  def diff_times: (List[Aexp], String) => Aexp = {
    case (aexplist, var0) =>
      {
        aexplist match {
          case Nil() => { Const(0) }
          case Cons(hd, tl) => {
            Sum(
              List(Times(List(diff(hd, var0)) ++ tl),
               Times(List(hd, diff_times(tl, var0)))))
          }
        }
    }
  }
  def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(num) => { Const(0) }
          case Var(st) => { if (st == var0) Const(1) else Const(0) }
          case Power(st, num) => {
            
              if (
                st == var0
              ) {
                Times(List(Const(num), Power(st, num - 1))) 
              } else {
                Const(0)
              }
          }
          case Times(aexplist) => { diff_times(aexplist ++ List(Const(1)), var0)
          }
          case Sum(aexplist) => { Sum(diff_sum(aexplist, var0)) }
        }
    }
  }
}