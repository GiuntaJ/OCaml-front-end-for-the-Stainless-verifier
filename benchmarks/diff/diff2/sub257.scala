import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub257 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(e) => { Const(0) }
          case Var(e) => { if (e == x) Const(1) else Const(0) }
          case Power(s, e) => {
            
              if (
                e == 0
              ) {
                Const(0) 
              } else if (
                s != x
              ) {
                Const(0) 
              } else if (
                e == 2
              ) {
                Times(List(Const(2), Var(s))) 
              } else if (
                e == 1
              ) {
                Const(1) 
              } else {
                Times(List(Const(e), Power(s, e - 1)))
              }
          }
          case Times(l) => { Sum(diff_times(l, Nil(), x)) }
          case Sum(l) => { Sum(diff_sum(l, x)) }
        }
    }
  }
  def diff_times: (List[Aexp], List[Aexp], String) => List[Aexp] = {
    case (st, ed, x) =>
      {
        st match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => {
            Times(ed ++ List(diff(hd, x)) ++ tl) ::
            diff_times(tl, ed ++ List(hd), x)
          }
        }
    }
  }
  def diff_sum: (List[Aexp], String) => List[Aexp] = {
    case (lst, x) =>
      {
        lst match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => { diff(hd, x) :: diff_sum(tl, x) }
        }
    }
  }
    
  /*
  diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;
  
  diff (Sum [Power ("x", 2); Times [Const 2; Var "x"; Var "y"]; Power("y", 2)], "x");;
  */
}