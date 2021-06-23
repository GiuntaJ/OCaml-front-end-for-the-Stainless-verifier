import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub142 {
  
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
          case Const(n) => { Const(0) }
          case Var(str) => { if (str == var0) Const(1) else Const(0) }
          case Power(str, n) => {
            
              if (
                str == var0
              ) {
                
                  if (
                    n == 0
                  ) {
                    Const(0) 
                  } else {
                    Times(List(Const(n), Power(str, n - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case _ => { Sum(product(Nil(), lst, var0)) }
            }
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case _ => { Sum(add(Nil(), lst, var0)) }
            }
          }
        }
    }
  }
  def product: (List[Aexp], List[Aexp], String) => List[Aexp] = {
    case (lst1, lst2, var0) =>
      {
        lst2 match {
          case Nil() => { Nil() }
          case Cons(h, t) => {
            Times(lst1 ++ List(diff(h, var0)) ++ t) ::
            product(lst1 ++ List(h), t, var0)
          }
        }
    }
  }
  def add: (List[Aexp], List[Aexp], String) => List[Aexp] = {
    case (lst1, lst2, var0) =>
      {
        lst2 match {
          case Nil() => { Nil() }
          case Cons(h, t) => { diff(h, var0) :: add(lst1 ++ List(h), t, var0) }
        }
    }
  }
}