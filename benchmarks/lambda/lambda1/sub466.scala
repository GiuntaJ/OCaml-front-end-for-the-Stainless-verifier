import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub466 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(inputmetro: Metro): Boolean = {
    val _2 = {
      def check(((m: Metro, validarea: List[Name]))): Boolean = {
        m match {
          case STATION(name) => { validarea.contains(name) }
          case AREA(name, metro) => { check(metro, name :: validarea) }
          case CONNECT(metro1, metro2) => {
            check(metro1, validarea) && check(metro2, validarea)
          }
        }
      }
      check(inputmetro, Nil())
    }
  }
}