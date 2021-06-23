import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub196 {
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
          def ae(f, isfactor) = {
            f match {
              case Sum(a) => {
                val _10 = {
                  def sumhelp(b) = {
                    b match {
                      case Nil() => { Nil() }
                      case Cons(hd, tl) => { ae(hd, false) :: sumhelp(tl) }
                    }
                  }
                  Sum(sumhelp(a))
                }
              }
              case Times(a) => {
                val _5 = {
                  val isx = val _6 = {
                    def xsearch(term) = {
                      term match {
                        case Nil() => { false }
                        case Cons(hd, tl) => {
                          hd match {
                            case Var(a) => { if (a == x) true else xsearch(tl) }
                            case Power(a, b) => {
                              
                                if (
                                  a == x
                                ) {
                                  if (b == 0) xsearch(tl) else true 
                                } else {
                                  xsearch(tl)
                                }
                            }
                            case _ => { xsearch(tl) }
                          }
                        }
                      }
                    }
                    xsearch(a)
                  }
                  val _7 = {
                    def timeshelp(b) = {
                      b match {
                        case Nil() => { Nil() }
                        case Cons(hd, tl) => { ae(hd, isx) :: timeshelp(tl) }
                      }
                    }
                    Times(timeshelp(a))
                  }
                }
              }
              case Power(a, b) => {
                
                  if (
                    a != x
                  ) {
                    if (isfactor) Power(a, b) else Const(0) 
                  } else if (
                    b == 1
                  ) {
                    Const(1) 
                  } else if (
                    b == 0
                  ) {
                    if (isfactor) Const(1) else Const(0) 
                  } else {
                    Times(List(Const(b), Power(a, b - 1)))
                  }
              }
              case Var(a) => {
                
                  if (
                    a == x
                  ) {
                    Const(1) 
                  } else if (
                    isfactor
                  ) {
                    Var(a) 
                  } else {
                    Const(0)
                  }
              }
              case Const(a) => { if (isfactor) Const(a) else Const(0) }
            }
          }
          ae(e, false)
        }
    }
  }
}