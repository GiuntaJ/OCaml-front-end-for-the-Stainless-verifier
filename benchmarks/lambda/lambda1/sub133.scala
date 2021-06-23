import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub133 {
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(tak: Metro): Boolean = {
    val _2 = {
      def makeList(a, lst) = {
        a match {
          case STATION(s) => { lst.contains(s) }
          case AREA(x, y) => { makeList(y, x :: lst) }
          case CONNECT(x, y) => { makeList(x, lst) && makeList(y, lst) }
        }
      }
      makeList(tak, Nil())
    }
  }
}