import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub225 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  val diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        val _2 = {
          def impl(_e) = {
            _e match {
              case Const(_) => { Const(0) }
              case Var(y) => { if (x == y) Const(1) else Const(0) }
              case Power(y, n) => {
                
                  if (
                    x == y
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
                        Times(List(Const(2), Var(y))) 
                      } else {
                        Times(List(Const(n), Power(y, n - 1)))
                      } 
                  } else {
                    Const(0)
                  }
              }
              case Times(l) => {
                val _11 = {
                  val tmp = l match {
                    case Nil() => { Const(0) }
                    case Cons(hd, tl) => {
                      val _14 = {
                        val res = impl(hd)
                        val _15 = {
                          val diff_tl_part = if (hd == Const(1)) impl(Times(tl)) else Times(List(hd, impl(Times(tl))))
                          
                            if (
                              res == Const(0)
                            ) {
                              diff_tl_part 
                            } else if (
                              diff_tl_part == Const(0)
                            ) {
                              Times(impl(hd) :: tl) 
                            } else {
                              Sum(List(Times(impl(hd) :: tl), diff_tl_part))
                            }
                        }
                      }
                    }
                  }
                  tmp
                }
              }
              case Sum(l) => {
                val _5 = {
                  val tmp = l match {
                    case Nil() => { Const(0) }
                    case Cons(hd, tl) => {
                      val _8 = {
                        val res = impl(hd)
                        
                          if (
                            res == Const(0)
                          ) {
                            impl(Sum(tl)) 
                          } else {
                            Sum(List(impl(hd), impl(Sum(tl))))
                          }
                      }
                    }
                  }
                  tmp
                }
              }
            }
          }
          impl(e)
        }
    }
  }
}