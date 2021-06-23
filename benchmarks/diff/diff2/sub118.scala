import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub118 {
  /*
    1. You can modify the given function specifications as recursive.
    2. Do not modify the function names or types.
    3. It is free to define any helper functions.
  */
  
  sealed case class NotImplemented() extends Exception {}
  
  
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
          case Var(k) => { if (k == var0) Const(1) else Const(1) }
          case Power(k, n) => {
            if (k == var0) Times(List(Const(n), Power(k, n - 1))) else Const(0)
          }
          case Times(Cons(hd, tl)) => {
            
              if (
                tl == Nil()
              ) {
                diff(hd, var0) 
              } else if (
                diff(hd, var0) == Const(0)
              ) {
                Times(List(hd, diff(Times(tl), var0))) 
              } else {
                Times(List(diff(hd, var0), diff(Times(tl), var0)))
              }
          }
          case Sum(Cons(hd, tl)) => {
            
              if (
                tl == Nil()
              ) {
                diff(hd, var0) 
              } else {
                Sum(List(diff(hd, var0), diff(Sum(tl), var0)))
              }
          }
          case _ => { assert(false, "NotImplemented") }
        }
    }
  } /* TODO */
}