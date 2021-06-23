import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub51 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((a, b))) = {
    val _2 = {
      def realdiff(((a, b))) = {
        a match {
          case CONST(x) => { CONST(0) }
          case VAR(x) => { if (x == b) CONST(1) else CONST(0) }
          case POWER(x, y) => {
            if (x == b) TIMES(List(CONST(y), POWER(x, y - 1))) else CONST(0)
          }
          case TIMES(lst) => {
            lst match {
              case Cons(hd, Nil()) => { realdiff(hd, b) }
              case Cons(hd, tl) => {
                SUM(
                  List(TIMES(List(hd, realdiff(TIMES(tl), b))),
                   TIMES(List(realdiff(hd, b), TIMES(tl)))))
              }
              case Nil() => { assert(false, "InvalidArgument") }
            }
          }
          case SUM(lst) => {
            lst match {
              case Cons(hd, Nil()) => { realdiff(hd, b) }
              case Cons(hd, tl) => {
                SUM(List(realdiff(hd, b), realdiff(SUM(tl), b)))
              }
              case Nil() => { assert(false, "InvalidArgument") }
            }
          }
        }
      }
      val _3 = {
        def maketime(((a, b))) = {
          b match {
            case TIMES(lst) => { TIMES(a :: lst) }
            case _ => { TIMES(List(a, b)) }
          }
        }
        val _4 = {
          def makesum(((a, b))) = {
            b match {
              case SUM(lst) => { SUM(a :: lst) }
              case _ => { SUM(List(a, b)) }
            }
          }
          val _5 = {
            def simplifier(a) = {
              a match {
                case CONST(x) => { CONST(x) }
                case VAR(x) => { VAR(x) }
                case POWER(x, y) => { POWER(x, y) }
                case TIMES(lst) => {
                  lst match {
                    case Cons(hd, Nil()) => { simplifier(hd) }
                    case Cons(hd, tl) => {
                      
                        if (
                          simplifier(TIMES(tl)) == CONST(1)
                        ) {
                          simplifier(hd) 
                        } else if (
                          simplifier(TIMES(tl)) == CONST(0)
                        ) {
                          CONST(0) 
                        } else if (
                          simplifier(hd) == CONST(0)
                        ) {
                          CONST(0) 
                        } else if (
                          simplifier(hd) == CONST(1)
                        ) {
                          simplifier(TIMES(tl)) 
                        } else {
                          maketime(simplifier(hd), simplifier(TIMES(tl)))
                        }
                    }
                    case Nil() => { assert(false, "InvalidArgument") }
                  }
                }
                case SUM(lst) => {
                  lst match {
                    case Cons(hd, Nil()) => { simplifier(hd) }
                    case Cons(hd, tl) => {
                      
                        if (
                          simplifier(SUM(tl)) == CONST(0)
                        ) {
                          simplifier(hd) 
                        } else if (
                          simplifier(hd) == CONST(0)
                        ) {
                          simplifier(SUM(tl)) 
                        } else {
                          makesum(simplifier(hd), simplifier(SUM(tl)))
                        }
                    }
                    case Nil() => { assert(false, "InvalidArgument") }
                  }
                }
              }
            }
            simplifier(realdiff(a, b))
          }
        }
      }
    }
  }
  		   
}