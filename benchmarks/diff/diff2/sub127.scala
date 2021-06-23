import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub127 {
  
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
          case Var(v) => { if (var0 == v) Const(1) else Const(0) }
          case Power(v, n) => {
            
              if (
                n > 1 && var0 == v
              ) {
                Times(List(Const(n), Power(v, n - 1))) 
              } else if (
                n eq 1 && var0 == v
              ) {
                Const(1) 
              } else {
                Const(0)
              }
          }
          case Times(lst) => {
            val _5 = {
              def diffTimes: (List[Aexp], String, List[Aexp], List[Aexp]) => Aexp = {
                case (lst, var0, sumlst, prelst) =>
                  {
                    lst match {
                      case Nil() => { Sum(sumlst) }
                      case Cons(n, lst2) => {
                        val _8 = {
                          val a = Times(prelst ++ diff(n, var0) :: lst2)
                          diffTimes(
                            lst2, var0, sumlst ++ List(a), prelst ++ List(n))
                        }
                      }
                    }
                }
              }
              diffTimes(lst, var0, Nil(), Nil())
            }
          }
          case Sum(lst) => {
            val _2 = {
              def diffSum: (List[Aexp], String, List[Aexp]) => Aexp = {
                case (lst, var0, sumlst) =>
                  {
                    lst match {
                      case Nil() => { Sum(sumlst) }
                      case Cons(n, lst2) => {
                        diffSum(lst2, var0, sumlst ++ List(diff(n, var0)))
                      }
                    }
                }
              }
              diffSum(lst, var0, Nil())
            }
          }
        }
    }
  }
}