import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub94 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff: (Ae, String) => Ae = {
    case (a, str) =>
      {
        a match {
          case CONST(n) => { CONST(0) }
          case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
          case POWER(s, n) => {
            
              if (
                s == str
              ) {
                
                  if (
                    n == 2
                  ) {
                    TIMES(List(CONST(2), VAR(s))) 
                  } else if (
                    n == 1
                  ) {
                    CONST(1) 
                  } else if (
                    n == 0
                  ) {
                    CONST(0) 
                  } else {
                    TIMES(List(CONST(n), POWER(s, n - 1)))
                  } 
              } else {
                CONST(0)
              }
          }
          case TIMES(Nil()) => { assert(false, "InvalidArgument") }
          case TIMES(aes) => {
            val _5 = {
              def clean(aes) = {
                
                  if (
                    aes.contains(CONST(0))
                  ) {
                    CONST(0) 
                  } else {
                    val _9 = {
                      val filt = aes.filter(( (x) => { x != CONST(1) } ))
                      
                        if (
                          filt.length == 0
                        ) {
                          CONST(0) 
                        } else if (
                          filt.length == 1
                        ) {
                          filt.head 
                        } else {
                          TIMES(filt)
                        }
                    }
                  }
              }
              val _10 = {
                val new_aes = aes.map((
                  (x) =>
                    {
                      clean(
                        aes.map(( (y) => { if (x == y) diff(y, str) else y } )))
                  }
                )).filter(( (n) => { n != CONST(0) } ))
                
                  if (
                    new_aes.length == 0
                  ) {
                    CONST(0) 
                  } else if (
                    new_aes.length == 1
                  ) {
                    new_aes.head 
                  } else {
                    SUM(new_aes)
                  }
              }
            }
          }
          case SUM(Nil()) => { assert(false, "InvalidArgument") }
          case SUM(aes) => {
            val _2 = {
              val new_aes = aes.map(( (x) => { diff(x, str) } )).filter(( (n) => { n != CONST(0) } ))
              
                if (
                  new_aes.length == 0
                ) {
                  CONST(0) 
                } else if (
                  new_aes.length == 1
                ) {
                  new_aes.head 
                } else {
                  SUM(new_aes)
                }
            }
          }
        }
    }
  }
  			
}