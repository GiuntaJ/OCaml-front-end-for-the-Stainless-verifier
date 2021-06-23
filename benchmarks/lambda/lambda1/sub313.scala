import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub313 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def loop(mt, li) = {
        mt match {
          case STATION(name) => { li.exists(( (x) => { x == name } )) }
          case AREA(name, metro) => { loop(metro, li ++ List(name)) }
          case CONNECT(mt1, mt2) => { loop(mt1, li) && loop(mt2, li) }
        }
      }
      loop(metro, Nil())
    }
  }
}
