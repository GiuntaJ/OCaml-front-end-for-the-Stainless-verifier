import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub18 {
  sealed case class Error(param0: String) extends Exception {}
  
  /* EX8 : checkMetro */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  /* definition of metro */
  
  def checkMetro(a: Metro): Boolean = {
    val _2 = {
      def check(mt, lst) = {
        mt match {
          case STATION(name) => { lst.contains(name) }
          case AREA(nm, mt) => { check(mt, nm :: lst) }
          case CONNECT(m1, m2) => { check(m1, lst) && check(m2, lst) }
        }
      }
      check(a, Nil())
    }
  }
}