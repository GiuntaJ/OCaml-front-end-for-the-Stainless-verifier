import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub5 {
  
  type Name = String
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def checkMetro(e: Metro): Boolean = {
    val _2 = {
      def checkMetro_aux(f) = {
        f match {
          case (STATION(a), l) => {
            
              if (
                l == Nil()
              ) {
                false 
              } else if (
                a == l.head
              ) {
                true 
              } else {
                checkMetro_aux(STATION(a), l.tail)
              }
          }
          case (AREA(a, b), l) => { checkMetro_aux(b, a :: l) }
          case (CONNECT(a, b), l) => {
            checkMetro_aux(a, l) && checkMetro_aux(b, l)
          }
        }
      }
      checkMetro_aux(e, Nil())
    }
  }
}