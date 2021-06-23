import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub144 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        (exp, var0) match {
          case (Const(n), str) => { Const(0) }
          case (Var(x), str) => { if (str == x) Const(1) else Const(0) }
          case (Power(x, n), str) => {
            if (str == x) Times(List(Const(n), Power(x, n - 1))) else Const(0)
          }
          case (Times(l), str) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                
                  if (
                    diff(hd, str) == Const(0)
                  ) {
                    val _6 = {
                      val lst = List(hd) ++ List(diff(Times(tl), str))
                      Times(lst)
                    } 
                  } else {
                    val _7 = {
                      val lst = List(diff(hd, str)) ++ tl
                      Times(lst)
                    }
                  }
              }
            }
          }
          case (Sum(l), str) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                val _2 = {
                  val lst = List(diff(hd, str)) ++ List(diff(Sum(tl), str))
                  Sum(lst)
                }
              }
            }
          }
          case _ => { assert(false, "Failure with wrong expression") }
        }
    }
  }
}