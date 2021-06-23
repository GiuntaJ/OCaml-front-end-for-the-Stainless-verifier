import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub257 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def checkMetroInternal(((metro: Metro, validNames: List[Name]))) = {
        metro match {
          case STATION(id) => { validNames.contains(id) }
          case AREA(id, metro) => { checkMetroInternal(metro, id :: validNames)
          }
          case CONNECT(metro1, metro2) => {
            checkMetroInternal(metro1, validNames) &&
            checkMetroInternal(metro2, validNames)
          }
        }
      }
      checkMetroInternal(metro, Nil())
    }
  }
}