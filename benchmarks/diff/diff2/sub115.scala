import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub115 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        val _2 = {
          def t2(vl, var0) = {
            vl match {
              case Nil() => { Nil() }
              case Cons(hd, tl) => {
                List(Times(List(diff(hd, var0)) ++ tl),
                 Times(hd :: t2(tl, var0)))
              }
            }
          }
          val _3 = {
            def t(el, c, vl, var0) = {
              el match {
                case Nil() => { Times(List(Const(c), Sum(t2(vl, var0)))) }
                case Cons(hd, tl) => {
                  hd match {
                    case Const(x) => { t(tl, c * x, vl, var0) }
                    case a => { t(tl, c, vl ++ List(a), var0) }
                  }
                }
              }
            }
            val _4 = {
              def s(el, l, var0) = {
                el match {
                  case Nil() => { Sum(l) }
                  case Cons(hd, tl) => { s(tl, l ++ List(diff(hd, var0)), var0)
                  }
                }
              }
              exp match {
                case Const(x) => { Const(0) }
                case Var(x) => { if (x == var0) Const(1) else Const(0) }
                case Power(x, y) => {
                  
                    if (
                      x == var0
                    ) {
                      
                        if (
                          y == 0
                        ) {
                          Const(0) 
                        } else if (
                          y == 1
                        ) {
                          diff(Var(x), var0) 
                        } else if (
                          y == 2
                        ) {
                          Times(List(Const(2), Var(x))) 
                        } else {
                          Times(List(Const(y), Power(x, y - 1)))
                        } 
                    } else {
                      Const(0)
                    }
                }
                case Times(l) => { t(l, 1, Nil(), var0) }
                case Sum(l) => { s(l, Nil(), var0) }
              }
            }
          }
        }
    }
  }
}