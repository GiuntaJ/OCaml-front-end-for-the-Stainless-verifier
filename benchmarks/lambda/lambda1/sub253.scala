import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub253 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def checkMetroWithNames(metro, names) = {
        metro match {
          case STATION(name) => { names.exists(( (x) => { x == name } )) }
          case AREA(name, metro) => { checkMetroWithNames(metro, name :: names)
          }
          case CONNECT(metro0, metro1) => {
            checkMetroWithNames(metro0, names) &&
            checkMetroWithNames(metro1, names)
          }
        }
      }
      checkMetroWithNames(metro, Nil())
    }
  }
}