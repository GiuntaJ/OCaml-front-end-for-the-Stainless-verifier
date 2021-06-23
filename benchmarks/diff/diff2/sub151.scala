import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub151 {
  
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
          case Const(a) => { Const(0) }
          case Var(str) => { if (str == var0) Const(1) else Var(str) }
          case Power(str, a) => {
            
              if (
                str == var0
              ) {
                Times(List(Const(a), Power(str, a - 1))) 
              } else {
                Power(str, a)
              }
          }
          case Times(lst) => { Sum(difftime(lst, var0)) }
          case Sum(lst) => { Sum(diffsum(lst, var0)) }
        }
    }
  }
  def diffsum(((lst, var0))) = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => { List(diff(hd, var0)) ++ diffsum(tl, var0) }
    }
  }
  def difftime(((lst, var0))) = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        
          if (
            tl == Nil()
          ) {
            List(Times(List(diff2(hd, var0)) ++ tl)) 
          } else {
            List(Times(List(diff2(hd, var0)) ++ tl)) ++
            List(Times(List(hd) ++ List(Sum(difftime(tl, var0)))))
          }
      }
    }
  }
  def diff2: (Aexp, String) => Aexp = {
    case (hd, var0) =>
      {
        hd match {
          case Const(a) => { Const(0) }
          case Var(str) => { if (str == var0) Const(1) else Const(0) }
          case Power(str, a) => {
            
              if (
                str == var0
              ) {
                Times(List(Const(a), Power(str, a - 1))) 
              } else {
                Const(0)
              }
          }
          case Times(lst) => { diff(Times(lst), var0) }
          case Sum(lst) => { diff(Sum(lst), var0) }
        }
    }
  }
}