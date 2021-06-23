import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub50 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((ae, str))) = {
    ae match {
      case CONST(x) => { CONST(0) }
      case VAR(x) => { if (x == str) CONST(1) else CONST(0) }
      case POWER(s, i) => {
        
          if (
            s == str
          ) {
            
              if (
                i == 1
              ) {
                CONST(1) 
              } else if (
                i == 0
              ) {
                CONST(0) 
              } else {
                TIMES(List(CONST(i), POWER(s, i - 1)))
              } 
          } else {
            CONST(0)
          }
      }
      case TIMES(l) => {
        
          if (
            l == Nil()
          ) {
            assert(false, "InvalidArgument") 
          } else if (
            check(l, str) == true
          ) {
            SUM(gettimes(l, str)) 
          } else {
            CONST(0)
          }
      }
      case SUM(l) => {
        
          if (
            l == Nil()
          ) {
            assert(false, "InvalidArgument") 
          } else {
            SUM(getsum(l, str))
          }
      }
    }
  }
  def check(((l, str))) = {
    l match {
      case Cons(hd, tl) => {
        hd match {
          case VAR(x) => { if (x == str) true else check(tl, str) }
          case POWER(x, i) => { if (x == str) true else check(tl, str) }
          case TIMES(x) | SUM(x) => {
            if (check(x, str) == false) check(tl, str) else true
          }
          case _ => { check(tl, str) }
        }
      }
      case Nil() => { false }
    }
  }
  def gettimes(((l, str))) = {
    l match {
      case Cons(hd, tl) => {
        
          if (
            tl == Nil()
          ) {
            List(diff(hd, str)) 
          } else {
            List(TIMES(List(diff(hd, str)) ++ tl)) ++
            List(TIMES(List(hd) ++ List(SUM(gettimes(tl, str)))))
          }
      }
      case Nil() => { Nil() }
    }
  }
  def getsum(((l, str))) = {
    l match {
      case Cons(hd, tl) => { List(diff(hd, str)) ++ getsum(tl, str) }
      case Nil() => { Nil() }
    }
  }
  	
}