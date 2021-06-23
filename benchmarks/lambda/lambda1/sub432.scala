import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub432 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val startlist: List[A] = Nil()
  
  def checkMetro1(m: Metro, listofmetro: List[Name]): Boolean = {
    m match {
      case STATION(name1) => { listofmetro.contains(name1) }
      case AREA(n1, metro) => { checkMetro1(metro, n1 :: listofmetro) }
      case CONNECT(metro1, metro2) => {
        checkMetro1(metro1, listofmetro) && checkMetro1(metro2, listofmetro)
      }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetro1(m, startlist) }
}