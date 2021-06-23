import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub291 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val areas: List[String] = Nil()
  
  def checkM: (Metro, List[String]) => Boolean = {
    case (metro, areas) =>
      {
        metro match {
          case STATION(n) => { areas.contains(n) }
          case AREA(n, m) => {
            val _2 = {
              val areas = n :: areas
              checkM(m, areas)
            }
          }
          case CONNECT(m1, m2) => { checkM(m1, areas) && checkM(m2, areas) }
        }
    }
  }
  
  def checkMetro: Metro => Boolean = ( (metro) => { checkM(metro, areas) } )
}