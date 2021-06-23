import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub236 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /* Make a list and match */
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetro_list(m: Metro, l: List[Name]): Boolean = {
        m match {
          case STATION(name1) => { l.contains(name1) }
          case AREA(name1, metro1) => { checkMetro_list(metro1, name1 :: l) }
          case CONNECT(metro1, metro2) => {
            checkMetro_list(metro1, l) && checkMetro_list(metro2, l)
          }
        }
      }
      checkMetro_list(m, Nil())
    }
  }
}