import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub124 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  	def map(f, ((l, var0))) = {
    l match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => { f(hd, var0) :: map(f, tl, var0) }
    }
  }
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(a) => { if (var0 == a) Const(1) else Const(0) }
          case Power(a, n) => {
            
              if (
                var0 == a
              ) {
                n match {
                  case 0 => { Const(0) }
                  case 1 => { Const(1) }
                  case 2 => { Times(List(Const(2), Var(a))) }
                  case _ => { Times(List(Const(n), Power(a, n - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Sum(l) => { Sum(map(diff, l, var0)) }
          case Times(l) => {
            l match {
              case Nil() => { Const(1) }
              case Cons(hd, tl) => {
                hd match {
                  case Power(a, n) => {
                    
                      if (
                        var0 == a
                      ) {
                        
                          if (
                            tl == Nil()
                          ) {
                            diff(Power(a, n), var0) 
                          } else {
                            Times(List(diff(Power(a, n), var0), Times(tl)))
                          } 
                      } else if (
                        tl == Nil()
                      ) {
                        hd 
                      } else {
                        Times(List(hd, diff(Times(tl), var0)))
                      }
                  }
                  case Var(a) => {
                    
                      if (
                        var0 == a
                      ) {
                        
                          if (
                            tl == Nil()
                          ) {
                            Const(1) 
                          } else {
                            Times(List(Const(1), Times(tl)))
                          } 
                      } else {
                        Times(List(hd, diff(Times(tl), var0)))
                      }
                  }
                  case _ => {
                    
                      if (
                        tl == Nil()
                      ) {
                        Const(1) 
                      } else {
                        Times(List(hd, diff(Times(tl), var0)))
                      }
                  }
                }
              }
            }
          }
        }
    }
  }
}