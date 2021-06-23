import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub3 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  def crazy2add(((a: Crazy2, b: Crazy2))) = {
    val _2 = {
      def crazy2val(k: Crazy2) = {
        k match {
          case ZERO(k_0) => { 2 * crazy2val(k_0) }
          case ONE(k_0) => { 1 + 2 * crazy2val(k_0) }
          case MONE(k_0) => { -(1) + 2 * crazy2val(k_0) }
          case NIL => { 0 }
        }
      }
      val _3 = {
        val sum = crazy2val(a) + crazy2val(b)
        val _4 = {
          def matpl(x: Int63) = {
            
              if (
                x == 0
              ) {
                ZERO(NIL) 
              } else if (
                x == 1
              ) {
                ONE(NIL) 
              } else if (
                x % 2 == 0
              ) {
                ZERO(matpl(x / 2)) 
              } else {
                ONE(matpl(x / 2))
              }
          }
          val _5 = {
            def matmi(x: Int63) = {
              
                if (
                  x == 0
                ) {
                  ZERO(NIL) 
                } else if (
                  x == 1
                ) {
                  MONE(NIL) 
                } else if (
                  x % 2 == 0
                ) {
                  ZERO(matmi(x / 2)) 
                } else {
                  MONE(matmi(x / 2))
                }
            }
            if (sum > 0) matpl(sum) else matmi(-(1) * sum)
          }
        }
      }
    }
  }
}
