import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub79 {
  
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
          case Var(x) => { if (x == var0) Const(1) else Const(0) }
          case Power(x, y) => {
            if (x == var0) Times(List(Const(y), Power(x, y - 1))) else Const(0)
          }
          case Const(x) => { Const(0) }
          case Sum(x) => {
            val _6 = {
              def sum: List[Aexp] => List[Aexp] = (
                (li) =>
                  {
                    li match {
                      case Nil() => { Nil() }
                      case Cons(hd, tl) => { diff(hd, var0) :: sum(tl) }
                    }
                }
              )
              Sum(sum(x))
            }
          }
          case Times(x) => {
            val _2 = {
              def product: (List[Aexp], Int63) => List[Aexp] = {
                case (li, n) =>
                  {
                    li match {
                      case Nil() => { Nil() }
                      case Cons(hd, tl) => {
                        
                          if (
                            n == 0
                          ) {
                            diff(hd, var0) :: product(tl, n - 1) 
                          } else {
                            hd :: product(tl, n - 1)
                          }
                      }
                    }
                }
              }
              val _3 = {
                def time: (List[Aexp], List[Aexp], Int63) => List[Aexp] = {
                  case (li, chli, n) =>
                    {
                      chli match {
                        case Nil() => { Nil() }
                        case Cons(hd, tl) => {
                          Times(product(li, n)) :: time(li, tl, n + 1)
                        }
                      }
                  }
                }
                Sum(time(x, x, 0))
              }
            }
          }
        }
    }
  }
}