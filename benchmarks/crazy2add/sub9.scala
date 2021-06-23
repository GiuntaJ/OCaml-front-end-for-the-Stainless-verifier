import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub9 {
  sealed case class Error(param0: String) extends Exception {}
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  def crazy2add(((a, b))) = {
    val _2 = {
      def crazy2val(c) = {
        val _5 = {
          def crazy2val_sub(c3) = {
            c3 match {
              case NIL => { 0 }
              case ZERO(c1) => { crazy2val_sub(c1) * 2 }
              case ONE(c1) => { crazy2val_sub(c1) * 2 + 1 }
              case MONE(c1) => { crazy2val_sub(c1) * 2 - 1 }
            }
          }
          
            if (
              c == NIL
            ) {
              assert(false, "Error with invalid arg ") 
            } else {
              crazy2val_sub(c)
            }
        }
      }
      val _6 = {
        def val2crazy(c) = {
          
            if (
              c == 0
            ) {
              NIL 
            } else if (
              c > 0
            ) {
              c % 2 match {
                case 0 => { ZERO(val2crazy(c / 2)) }
                case _ => { ONE(val2crazy((c - 1) / 2)) }
              } 
            } else {
              c % 2 match {
                case 0 => { ZERO(val2crazy(c / 2)) }
                case _ => { MONE(val2crazy((c + 1) / 2)) }
              }
            }
        }
        
          if (
            crazy2val(a) + crazy2val(b) == 0
          ) {
            ZERO(NIL) 
          } else {
            val2crazy(crazy2val(a) + crazy2val(b))
          }
      }
    }
  }
  		
  			
  			
  		
}