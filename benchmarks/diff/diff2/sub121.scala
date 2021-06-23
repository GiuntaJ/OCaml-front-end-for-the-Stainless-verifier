import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub121 {
  
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
          case Var(k) => { if (k == var0) Const(1) else Var(k) }
          case Power(s, n) => {
            
              if (
                s == var0
              ) {
                
                  if (
                    n == 0
                  ) {
                    Const(0) 
                  } else {
                    Times(List(Power(s, n - 1), Const(n)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(li) => {
            val _5 = {
              def timesfun(exp, var0) = {
                exp match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => {
                    hd match {
                      case Const(n) => { List(Const(n)) ++ timesfun(tl, var0) }
                      case Var(k) => {
                        List(diff(hd, var0)) ++ timesfun(tl, var0)
                      }
                      case Power(s, n) => {
                        
                          if (
                            s == var0
                          ) {
                            
                              if (
                                n == 0
                              ) {
                                List(Const(0)) ++ timesfun(tl, var0) 
                              } else {
                                List(Times(List(Power(s, n - 1), Const(n)))) ++
                                timesfun(tl, var0)
                              } 
                          } else {
                            List(Const(1)) ++ timesfun(tl, var0)
                          }
                      }
                      case Times(li) => {
                        List(diff(hd, var0)) ++ timesfun(tl, var0)
                      }
                      case Sum(li) => {
                        List(diff(hd, var0)) ++ timesfun(tl, var0)
                      }
                    }
                  }
                }
              }
              Times(timesfun(li, var0))
            }
          }
          case Sum(li) => {
            val _2 = {
              def sumfun(exp) = {
                exp match {
                  case Nil() => { Nil() }
                  case Cons(hd, tl) => { List(diff(hd, var0)) ++ sumfun(tl) }
                }
              }
              Sum(sumfun(li))
            }
          }
        }
    }
  }
}