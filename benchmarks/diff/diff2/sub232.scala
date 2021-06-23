import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub232 {
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
          def d(exp, isTimesWithX) = {
            exp match {
              case Sum(l) => {
                val _10 = {
                  def iter(tmp) = {
                    tmp match {
                      case Nil() => { Nil() }
                      case Cons(hd, tl) => { d(hd, false) :: iter(tl) }
                    }
                  }
                  Sum(iter(l))
                }
              }
              case Times(l) => {
                val _5 = {
                  val tmpBool = val _6 = {
                    def iter(tmp) = {
                      tmp match {
                        case Nil() => { false }
                        case Cons(hd, tl) => {
                          hd match {
                            case Var(a) => { if (a == x) true else iter(tl) }
                            case Power(a, b) => {
                              
                                if (
                                  a == x
                                ) {
                                  if (b == 0) iter(tl) else true 
                                } else {
                                  iter(tl)
                                }
                            }
                            case _ => { iter(tl) }
                          }
                        }
                      }
                    }
                    iter(l)
                  }
                  val _7 = {
                    def iter(tmp) = {
                      tmp match {
                        case Nil() => { Nil() }
                        case Cons(hd, tl) => { d(hd, tmpBool) :: iter(tl) }
                      }
                    }
                    Times(iter(l))
                  }
                }
              }
              case Power(a, b) => {
                
                  if (
                    a == x
                  ) {
                    
                      if (
                        b == 1
                      ) {
                        Const(1) 
                      } else if (
                        b == 0
                      ) {
                        if (isTimesWithX) Const(1) else Const(0) 
                      } else {
                        Times(List(Const(b), Power(a, b - 1)))
                      } 
                  } else if (
                    isTimesWithX
                  ) {
                    Power(a, b) 
                  } else {
                    Const(0)
                  }
              }
              case Var(a) => {
                
                  if (
                    a == x
                  ) {
                    Const(1) 
                  } else if (
                    isTimesWithX
                  ) {
                    Var(a) 
                  } else {
                    Const(0)
                  }
              }
              case Const(a) => { if (isTimesWithX) Const(a) else Const(0) }
            }
          }
          d(e, false)
        }
    }
  }
}