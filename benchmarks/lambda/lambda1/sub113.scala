import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub113 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def checkiter(((metro, arealist))) = {
        (metro, arealist) match {
          case (STATION(name), arealist) => { arealist.contains(name) }
          case (AREA(areaname, next), arealist) => {
            checkiter(next, areaname :: arealist)
          }
          case (CONNECT(metro1, metro2), arealist) => {
            (checkiter(metro1, arealist), checkiter(metro2, arealist)) match {
              case (true, true) => { true }
              case _ => { false }
            }
          }
        }
      }
      checkiter(metro, Nil())
    }
  }
}