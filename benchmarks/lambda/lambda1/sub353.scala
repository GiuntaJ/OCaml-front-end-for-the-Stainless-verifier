import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub353 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def checkMetro_helper(m, name_list) = {
        m match {
          case STATION(name) => { name_list.contains(name) }
          case AREA(name, m1) => { checkMetro_helper(m1, name :: name_list) }
          case CONNECT(m1, m2) => {
            checkMetro_helper(m1, name_list) && checkMetro_helper(m2, name_list)
          }
        }
      }
      checkMetro_helper(metro, Nil())
    }
  }
}
