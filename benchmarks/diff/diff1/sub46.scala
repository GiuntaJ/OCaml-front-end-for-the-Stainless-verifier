import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub46 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((ae, str))) = {
    ae match {
      case CONST(d) => { CONST(0) }
      case VAR(s) => { if (str == s) CONST(1) else CONST(0) }
      case POWER(s, d) => {
        
          if (
            str == s
          ) {
            
              if (
                d == 0
              ) {
                CONST(0) 
              } else if (
                d == 1
              ) {
                CONST(1) 
              } else if (
                d == 2
              ) {
                TIMES(List(CONST(2), VAR(s))) 
              } else {
                TIMES(List(CONST(d), POWER(s, d - 1)))
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
          } else {
            val _7 = {
              def diff_nth_ae(i, ae_list) = {
                
                  if (
                    ae_list == Nil()
                  ) {
                    assert(false, "InvalidArgument") 
                  } else if (
                    i == 0
                  ) {
                    diff(ae_list.head, str) :: ae_list.tail 
                  } else {
                    ae_list.head :: diff_nth_ae(i - 1, ae_list.tail)
                  }
              }
              val _8 = {
                def diff_times_internal(n, ae_list) = {
                  
                    if (
                      n < ae_list.length
                    ) {
                      TIMES(diff_nth_ae(n, ae_list)) ::
                      diff_times_internal(n + 1, ae_list) 
                    } else {
                      Nil()
                    }
                }
                SUM(diff_times_internal(0, l))
              }
            }
          }
      }
      case SUM(l) => {
        
          if (
            l == Nil()
          ) {
            assert(false, "InvalidArgument") 
          } else {
            val _3 = {
              def diff_internal(s, e) = { diff(e, s) }
              SUM(l.map(diff_internal(str)))
            }
          }
      }
    }
  }
}