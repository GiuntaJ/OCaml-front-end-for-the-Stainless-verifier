import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub48 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetro(m, pl) = {
        m match {
          case STATION(n) => { pl.contains(n) }
          case AREA(n, m1) => { checkMetro(m1, n :: pl) }
          case CONNECT(m1, m2) => { checkMetro(m1, pl) && checkMetro(m2, pl) }
        }
      }
      checkMetro(m, Nil())
    }
  }
  
  /*
  AREA("a", STATION "a")
  AREA("a", AREA("a", STATION "a"))
  AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
  AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
  
  AREA("a", STATION "b")
  AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
  AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
  */
}
