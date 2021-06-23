import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub147 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def length(lst) = {
    lst match {
      case Nil() => { 0 }
      case Cons(hd, tl) => { 1 + length(tl) }
    }
  }
  
    def hd(lst) = {
    lst match {
      case Cons(hd, tl) => { hd }
    }
  }
  
    def tl(lst) = {
    lst match {
      case Cons(hd, tl) => { tl }
    }
  }
  
      def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Power(var1, int1) => {
            
              if (
                var1 == var0
              ) {
                
                  if (
                    int1 eq 2
                  ) {
                    Times(List(Const(int1), Var(var1))) 
                  } else {
                    Times(List(Const(int1), Power(var1, int1 - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Const(int1) => { Const(0) }
          case Var(var1) => { if (var1 == var0) Const(1) else Const(0) }
          case Sum(lst) => {
            val _5 = {
              def protoSum(tolst, fromlst, var0) = {
                fromlst match {
                  case Nil() => { Sum(tolst) }
                  case Cons(hd, tl) => {
                    protoSum(tolst ++ List(diff(hd, var0)), tl, var0)
                  }
                }
              }
              protoSum(Nil(), lst, var0)
            }
          }
          case Times(lst) => {
            val _2 = {
              def protoTime(tolst, fromlst, var0, pivot) = {
                
                  if (
                    pivot == 0
                  ) {
                    Sum(tolst) 
                  } else {
                    protoTime(
                      tolst ++
                      List(Times(diff(hd(fromlst), var0) :: tl(fromlst))),
                      tl(fromlst) ++ List(hd(fromlst)), var0, pivot - 1)
                  }
              }
              protoTime(Nil(), lst, var0, length(lst))
            }
          }
        }
    }
  }
}