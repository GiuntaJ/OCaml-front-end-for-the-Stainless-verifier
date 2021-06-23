import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub248 {
  type Name = String
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetro1(m, city) = {
        m match {
          case STATION(n) => { city.contains(n) }
          case AREA(n, m1) => {
            val _5 = {
              val city1 = n :: city
              checkMetro1(m1, city1)
            }
          }
          case CONNECT(m1, m2) => {
            checkMetro1(m1, city) && checkMetro1(m2, city)
          }
        }
      }
      checkMetro1(m, Nil())
    }
  }
}