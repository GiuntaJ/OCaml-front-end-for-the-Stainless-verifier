import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub130 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def listt1: (List[Aexp], String) => List[Aexp] = {
    case (lst, var0) =>
      {
        lst match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => { diff(hd, var0) :: listt1(tl, var0) }
        }
    }
  }
  def listt2: (List[Aexp], String, List[Aexp]) => List[Aexp] = {
    case (lst, var0, temp) =>
      {
        lst match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => {
            Times(temp ++ diff(hd, var0) :: tl) ::
            listt2(tl, var0, temp ++ List(hd))
          }
        }
    }
  }
  def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Sum(lst) => { Sum(listt1(lst, var0)) }
          case Times(lst) => { Sum(listt2(lst, var0, Nil())) }
          case Power(v, n) => {
            
              if (
                var0 == v
              ) {
                
                  if (
                    n > 2
                  ) {
                    Times(List(Const(n), Power(v, n - 1))) 
                  } else if (
                    n == 2
                  ) {
                    Times(List(Const(2), Var(v))) 
                  } else if (
                    n == 1
                  ) {
                    Const(1) 
                  } else {
                    Const(0)
                  } 
              } else {
                Const(0)
              }
          }
          case Const(x) => { Const(0) }
          case Var(v) => { if (var0 == v) Const(1) else Const(0) }
        }
    }
  }
}