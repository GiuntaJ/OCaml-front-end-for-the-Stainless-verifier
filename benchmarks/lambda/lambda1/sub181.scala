import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub181 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def checkMetro_sub(metro, env) = {
        metro match {
          case STATION(name) => { env.exists(( (x) => { x == name } )) }
          case AREA(name, m) => { checkMetro_sub(m, name :: env) }
          case CONNECT(m1, m2) => {
            checkMetro_sub(m1, env) && checkMetro_sub(m2, env)
          }
        }
      }
      checkMetro_sub(metro, Nil())
    }
  }
}