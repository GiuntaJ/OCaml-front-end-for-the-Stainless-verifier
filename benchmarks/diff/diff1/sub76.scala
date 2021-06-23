import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub76 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def simplify(a: Ae): Ae = {
    a match {
      case POWER(str, n) => {
        
          if (
            n == 0
          ) {
            CONST(1) 
          } else if (
            n == 1
          ) {
            VAR(str) 
          } else {
            a
          }
      }
      case TIMES(Cons(hd, tl)) => {
        
          if (
            tl == Nil()
          ) {
            simplify(hd) 
          } else if (
            simplify(hd) == CONST(0) || simplify(TIMES(tl)) == CONST(0)
          ) {
            CONST(0) 
          } else if (
            simplify(hd) == CONST(1)
          ) {
            simplify(TIMES(tl)) 
          } else if (
            simplify(TIMES(tl)) == CONST(1)
          ) {
            simplify(hd) 
          } else {
            TIMES(List(simplify(hd)) ++ List(simplify(TIMES(tl))))
          }
      }
      case SUM(Cons(hd, tl)) => {
        
          if (
            tl == Nil()
          ) {
            simplify(hd) 
          } else if (
            simplify(hd) == CONST(0)
          ) {
            simplify(SUM(tl)) 
          } else if (
            simplify(SUM(tl)) == CONST(0)
          ) {
            simplify(hd) 
          } else {
            SUM(List(simplify(hd)) ++ List(simplify(TIMES(tl))))
          }
      }
      case _ => { a }
    }
  }
  
  def diff(((a, s))) = {
    a match {
      case CONST(_) => { CONST(0) }
      case VAR(str) => { if (s == str) CONST(1) else CONST(0) }
      case POWER(str, n) => {
        
          if (
            s == str
          ) {
            simplify(TIMES(List(CONST(n)) ++ List(POWER(str, n - 1)))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(Cons(hd, tl)) => {
        simplify(
          SUM(
            List(TIMES(List(diff(hd, s)) ++ tl)) ++
            List(TIMES(
               List(hd) ++
               (
                  if (
                    tl == Nil()
                  ) {
                    List(CONST(0)) 
                  } else {
                    List(diff(TIMES(tl), s))
                  })))))
      }
      case TIMES(Nil()) => { assert(false, "InvalidArgument") }
      case SUM(Cons(hd, tl)) => {
        simplify(
          SUM(
            List(diff(hd, s)) ++
            (if (tl == Nil()) List(CONST(0)) else List(diff(SUM(tl), s)))))
      }
      case SUM(Nil()) => { assert(false, "InvalidArgument") }
    }
  }
}