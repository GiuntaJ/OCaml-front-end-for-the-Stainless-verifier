import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub22 {
  type Name = String
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetroA(((met, l))) = {
        met match {
          case STATION(n) => { l.exists(( (x) => { x == n } )) }
          case AREA(n, m) => { checkMetroA(m, n :: l) }
          case CONNECT(m1, m2) => {
            
              if (
                checkMetroA(m1, l)
              ) {
                if (checkMetroA(m2, l)) true else false 
              } else {
                false
              }
          }
        }
      }
      checkMetroA(m, Nil())
    }
  }
}