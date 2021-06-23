import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub168 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkM(((m, l))) = {
        m match {
          case STATION(n) => { l.contains(n) }
          case AREA(name, metro) => { checkM(metro, name :: l) }
          case CONNECT(metro1, metro2) => {
            checkM(metro1, l) && checkM(metro2, l)
          }
        }
      }
      checkM(m, Nil())
    }
  }
}