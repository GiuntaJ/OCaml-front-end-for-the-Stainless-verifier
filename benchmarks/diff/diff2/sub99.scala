import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub99 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    val diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        val _2 = {
          def f(e, v) = {
            e match {
              case Const(a) => { Const(0) }
              case Var(a) => { if (a == var0) Const(1) else Const(0) }
              case Power(x, y) => {
                
                  if (
                    x == var0
                  ) {
                    Times(List(Const(y), Power(x, y - 1))) 
                  } else {
                    Const(0)
                  }
              }
              case Sum(lst) => {
                lst match {
                  case Nil() => {
                    assert(false, "Failure with  No elements in the list! ")
                  }
                  case Cons(hd, tl) => {
                    
                      if (
                        tl eq Nil()
                      ) {
                        f(hd, var0) 
                      } else {
                        Sum(List(f(hd, var0)) ++ List(f(Sum(tl), var0)))
                      }
                  }
                }
              }
              case Times(lst) => {
                lst match {
                  case Nil() => {
                    assert(false, "Failure with  No elements in the list! ")
                  }
                  case Cons(hd, tl) => {
                    
                      if (
                        tl eq Nil()
                      ) {
                        f(hd, var0) 
                      } else {
                        hd match {
                          case Const(a) => {
                            Times(List(hd) ++ List(f(Times(tl), var0)))
                          }
                          case Var(a) => {
                            
                              if (
                                hd == Var(var0)
                              ) {
                                Sum(
                                  List(f(Times(tl), var0)) ++ List(hd) ++
                                  List(f(Times(tl), var0))) 
                              } else {
                                Times(List(hd) ++ List(f(Times(tl), var0)))
                              }
                          }
                          case Power(x, y) => {
                            
                              if (
                                Var(x) == Var(var0)
                              ) {
                                Sum(
                                  List(Times(List(f(hd, var0)) ++ tl)) ++
                                  List(Times(
                                     List(hd) ++ List(f(Times(tl), var0))))) 
                              } else {
                                Times(List(hd) ++ List(f(Times(tl), var0)))
                              }
                          }
                          case Sum(ist) => {
                            Sum(
                              List(Times(List(f(hd, var0)) ++ tl)) ++
                              List(Times(List(hd) ++ List(f(Times(tl), var0)))))
                          }
                          case Times(ist) => {
                            Sum(
                              List(Times(List(f(hd, var0)) ++ tl)) ++
                              List(Times(List(hd) ++ List(f(Times(tl), var0)))))
                          }
                        }
                      }
                  }
                }
              }
            }
          }
          f(exp, var0)
        }
    }
  }
}