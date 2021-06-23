import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub52 {
  /*2009-11718 2-2*/
  
  sealed case class InvalidArgument() extends Exception {}
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((ae, str))) = {
    val _2 = {
      def differ(((ae, str))) = {
        ae match {
          case CONST(a) => { CONST(0) }
          case VAR(str1) => { if (str == str1) CONST(1) else CONST(0) }
          case POWER(str1, a) => {
            
              if (
                str == str1
              ) {
                TIMES(List(CONST(a)) ++ List(POWER(str1, a - 1))) 
              } else {
                CONST(0)
              }
          }
          case TIMES(l) => {
            
              if (
                l == Nil()
              ) {
                assert(false, "InvalidArgument") 
              } else {
                SUM(times(l, str, Nil()))
              }
          }
          case SUM(l) => {
            
              if (
                l == Nil()
              ) {
                assert(false, "InvalidArgument") 
              } else {
                SUM(sum(l, str))
              }
          }
        }
      }
      def times(((ae, str, result))) = {
        ae match {
          case Cons(hd, tl) => {
            
              if (
                ae.length == result.length
              ) {
                result 
              } else {
                times(
                  tl ++ List(hd), str,
                  result ++ List(TIMES(List(diff(hd, str)) ++ tl)))
              }
          }
          case Nil() => { Nil() }
        }
      }
      def sum(((ae, str))) = {
        ae match {
          case Cons(hd, tl) => { List(diff(hd, str)) ++ sum(tl, str) }
          case Nil() => { Nil() }
        }
      }
      differ(ae, str)
    }
  }
}