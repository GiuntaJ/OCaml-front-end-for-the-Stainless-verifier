import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub122 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def diff: (Aexp, String) => Aexp = {
    case (aex, str) =>
      {
        aex match {
          case Const(n) => { Const(0) }
          case Var(a) => { if (a == str) Const(1) else Var(a) }
          case Power(a, b) => {
            
              if (
                a == str
              ) {
                Times(List(Const(b), Power(a, b - 1))) 
              } else {
                Power(a, b)
              }
          }
          case Times(a) => { tImes(a, str) }
          case Sum(a) => { sUm(a, str) }
        }
    }
  }
  def tImes: (List[Aexp], String) => Aexp = {
    case (lst, str) =>
      {
        lst match {
          case Nil() => { Const(1) }
          case Cons(h, Nil()) => { diff(h, str) }
          case Cons(hd, tl) => {
            Sum(
              List(Times(List(diff(hd, str)) ++ tl)) ++
              List(Times(List(hd) ++ List(tImes(tl, str)))))
          }
        }
    }
  }
  def sUm: (List[Aexp], String) => Aexp = {
    case (lst, str) =>
      {
        lst match {
          case Nil() => { Const(0) }
          case Cons(hd, tl) => { Sum(List(diff(hd, str)) ++ List(sUm(tl, str)))
          }
        }
    }
  }
}