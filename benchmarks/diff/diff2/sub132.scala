import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub132 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {} 
  
  def diff: (Aexp, String) => Aexp = (
    (f) =>
      {
        f match {
          case (exp, str) => {
            val _2 = {
              val e = exp
              val _3 = {
                val s0 = str
                val _4 = {
                  def diffsum: (List[Aexp], String) => List[Aexp] = {
                    case (l, str) =>
                      {
                        l match {
                          case Nil() => { Nil() }
                          case Cons(hd, tl) => {
                            
                              if (
                                hd == Const(0)
                              ) {
                                diffsum(tl, s0) 
                              } else {
                                diff(hd, s0) :: diffsum(tl, s0)
                              }
                          }
                        }
                    }
                  }
                  e match {
                    case Const(n) => { Const(0) }
                    case Var(s1) => { if (s1 == s0) Const(1) else Var(s1) }
                    case Power(s2, n) => {
                      
                        if (
                          s2 == s0
                        ) {
                          Times(List(Const(n), Power(s2, n - 1))) 
                        } else {
                          Power(s2, n)
                        }
                    }
                    case Sum(l) => { Sum(diffsum(l, s0)) }
                    case Times(l) => {
                      val _7 = {
                        def swap: (Aexp, List[Aexp]) => List[Aexp] = {
                          case (e, l0) =>
                            {
                              l0 match {
                                case Nil() => { Nil() }
                                case Cons(hd, tl) => {
                                  
                                    if (
                                      hd == e
                                    ) {
                                      diff(e, s0) :: tl 
                                    } else {
                                      hd :: swap(e, tl)
                                    }
                                }
                              }
                          }
                        }
                        val _8 = {
                          def difftimes: List[Aexp] => List[Aexp] = (
                            (l1) =>
                              {
                                l1 match {
                                  case Nil() => { Nil() }
                                  case Cons(hd, tl) => {
                                    Times(swap(hd, l)) :: difftimes(tl)
                                  }
                                }
                            }
                          )
                          Sum(difftimes(l))
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
  )
}
