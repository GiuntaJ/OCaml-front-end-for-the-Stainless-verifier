import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub100 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  sealed case class InvalidArgument() extends Exception {}
  def diff(((ae, str))) = {
    ae match {
      case CONST(num) => { CONST(0) }
      case VAR(str2) => { if (str == str2) CONST(1) else CONST(0) }
      case POWER(str2, num) => {
        
          if (
            str == str2
          ) {
            
              if (
                num == 1
              ) {
                CONST(1) 
              } else {
                TIMES(List(CONST(num), POWER(str, num - 1)))
              } 
          } else {
            CONST(0)
          }
      }
      case SUM(aelist) => {
        
          if (
            aelist.length == 0
          ) {
            assert(false, "InvalidArgument") 
          } else if (
            aelist.length == 1
          ) {
            diff(aelist.head, str) 
          } else {
            SUM(List(diff(aelist.head, str), diff(SUM(aelist.tail), str)))
          }
      }
      case TIMES(aelist) => {
        
          if (
            aelist.length == 0
          ) {
            assert(false, "InvalidArgument") 
          } else {
            val _3 = {
              def timesdiff(((aelist, indexnum, nownum))) = {
                
                  if (
                    aelist.length == 1
                  ) {
                    
                      if (
                        indexnum == nownum
                      ) {
                        diff(aelist.head, str) 
                      } else {
                        aelist.head
                      } 
                  } else if (
                    indexnum == 1 && aelist.length > nownum
                  ) {
                    
                      if (
                        indexnum == nownum
                      ) {
                        SUM(
                          List(TIMES(
                             List(diff(aelist.head, str),
                              timesdiff(aelist.tail, 2, nownum))),
                           timesdiff(aelist, 1, nownum + 1))) 
                      } else {
                        SUM(
                          List(TIMES(
                             List(aelist.head,
                              timesdiff(aelist.tail, 2, nownum))),
                           timesdiff(aelist, 1, nownum + 1)))
                      } 
                  } else if (
                    indexnum == nownum
                  ) {
                    TIMES(
                      List(diff(aelist.head, str),
                       timesdiff(aelist.tail, indexnum + 1, nownum))) 
                  } else {
                    TIMES(
                      List(aelist.head,
                       timesdiff(aelist.tail, indexnum + 1, nownum)))
                  }
              }
              timesdiff(aelist, 1, 1)
            }
          }
      }
    }
  }
}