import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub93 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  	def contain: (List[Aexp], String) => Boolean = {
    case (exp, var0) =>
      {
        (exp, var0) match {
          case (Nil(), x) => { false }
          case (Cons(Const(a), tl), x) => { contain(tl, x) }
          case (Cons(Var(a), tl), x) => { if (a == x) true else contain(tl, x) }
          case (Cons(Power(x, a), tl), y) => {
            if (x == y && a ne 0) true else contain(tl, x)
          }
          case (_, _) => { assert(false, "Failure with Invalid input") }
        }
    }
  }
  	  
  	def timesHelper: (List[Aexp], String) => List[Aexp] = {
    case (exp, var0) =>
      {
        (exp, var0) match {
          case (Nil(), x) => { Nil() }
          case (Cons(Const(a), tl), x) => { Const(a) :: timesHelper(tl, x) }
          case (Cons(Var(a), tl), x) => {
            
              if (
                a == x
              ) {
                Const(1) :: timesHelper(tl, x) 
              } else {
                Var(a) :: timesHelper(tl, x)
              }
          }
          case (Cons(Power(x, a), tl), y) => {
            
              if (
                x == y && a ne 0
              ) {
                Times(List(Const(a), Power(x, a - 1))) :: timesHelper(tl, y) 
              } else {
                Power(x, a) :: timesHelper(tl, y)
              }
          }
          case (_, _) => { assert(false, "Failure with Invalid input") }
        }
    }
  }
  		
  	def sumHelper: (List[Aexp], String) => List[Aexp] = {
    case (exp, var0) =>
      {
        (exp, var0) match {
          case (Nil(), x) => { Nil() }
          case (Cons(Const(a), tl), x) => { Const(0) :: sumHelper(tl, x) }
          case (Cons(Var(a), tl), x) => {
            if (a == x) Const(1) :: sumHelper(tl, x) else sumHelper(tl, x)
          }
          case (Cons(Power(x, a), tl), y) => {
            
              if (
                x == y && a ne 0
              ) {
                Times(List(Const(a), Power(x, a - 1))) :: sumHelper(tl, y) 
              } else {
                Const(0) :: sumHelper(tl, y)
              }
          }
          case (Cons(Times(t), tl), x) => {
            
              if (
                contain(t, x)
              ) {
                Times(timesHelper(t, x)) :: sumHelper(tl, x) 
              } else {
                Const(0) :: sumHelper(tl, x)
              }
          }
          case (_, _) => { assert(false, "Failure with Invalid input") }
        }
    }
  }
  	  
  	def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        (exp, var0) match {
          case (Const(a), x) => { Const(0) }
          case (Var(a), x) => { if (a == x) Const(1) else Const(0) }
          case (Power(x, a), y) => {
            
              if (
                x == y && a ne 0
              ) {
                Times(List(Const(a), Power(x, a - 1))) 
              } else {
                Const(0)
              }
          }
          case (Times(t), x) => {
            if (contain(t, x)) Times(timesHelper(t, x)) else Const(0)
          }
          case (Sum(s), x) => { Sum(sumHelper(s, x)) }
        }
    }
  }
}