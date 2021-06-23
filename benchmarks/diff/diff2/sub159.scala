import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub159 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def zero: Aexp => Boolean = (
    (exp) =>
      {
        exp match {
          case Const(n) => { if (n == 0) true else false }
          case Times(lst) => {
            lst match {
              case Nil() => { true }
              case Cons(hd, tl) => {
                
                  if (
                    zero(hd)
                  ) {
                    true 
                  } else if (
                    tl == Nil()
                  ) {
                    false 
                  } else {
                    zero(Times(tl))
                  }
              }
            }
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { true }
              case Cons(hd, tl) => { if (not(zero(hd))) false else zero(Sum(tl))
              }
            }
          }
          case _ => { false }
        }
    }
  )
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(str) => { if (str == var0) Const(1) else Const(0) }
          case Power(str, n) => {
            
              if (
                str == var0
              ) {
                
                  if (
                    n == 0
                  ) {
                    Const(0) 
                  } else if (
                    n == 1
                  ) {
                    Const(1) 
                  } else if (
                    n == 2
                  ) {
                    Times(List(Const(2), Var(var0))) 
                  } else {
                    Times(List(Const(n), Power(str, n - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                
                  if (
                    tl == Nil()
                  ) {
                    diff(hd, var0) 
                  } else {
                    val _9 = {
                      val product1 = Times(List(diff(hd, var0), Times(tl)))
                      val _10 = {
                        val zero1 = zero(product1)
                        val _11 = {
                          val product2 = Times(List(hd, diff(Times(tl), var0)))
                          val _12 = {
                            val zero2 = zero(product2)
                            
                              if (
                                zero1 && zero2
                              ) {
                                Const(0) 
                              } else if (
                                zero1
                              ) {
                                product2 
                              } else if (
                                zero2
                              ) {
                                product1 
                              } else {
                                Sum(List(product1, product2))
                              }
                          }
                        }
                      }
                    }
                  }
              }
            }
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                val _2 = {
                  val d1 = diff(hd, var0)
                  val _3 = {
                    val zero1 = zero(d1)
                    val _4 = {
                      val d2 = diff(Sum(tl), var0)
                      val _5 = {
                        val zero2 = zero(d2)
                        
                          if (
                            zero1 && zero2
                          ) {
                            Const(0) 
                          } else if (
                            zero1
                          ) {
                            d2 
                          } else if (
                            zero2
                          ) {
                            d1 
                          } else {
                            d2 match {
                              case Sum(lst2) => { Sum(d1 :: lst2) }
                              case _ => { Sum(List(d1, d2)) }
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
    }
  }
}