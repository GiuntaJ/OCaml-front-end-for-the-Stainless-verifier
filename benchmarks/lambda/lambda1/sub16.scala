import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub16 {
  /* School of Computer Science & Engineering
   * 2009-23151
   * 조성근
   * HW 1 - Exercise 8
   */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def myCheck(m, l) = {
        m match {
          case STATION(name) => { l.contains(name) }
          case AREA(name, metro) => { myCheck(metro, name :: l) }
          case CONNECT(metro1, metro2) => {
            myCheck(metro1, l) && myCheck(metro2, l)
          }
        }
      }
      myCheck(m, Nil())
    }
  }
}