import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub32 {
  sealed case class Error() extends Exception {}
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checkMetro_in(met, lst) = {
        met match {
          case STATION(name) => { lst.contains(name) }
          case AREA(name, met2) => { checkMetro_in(met2, lst ++ List(name)) }
          case CONNECT(met1, met2) => {
            checkMetro_in(met1, lst) && checkMetro_in(met2, lst)
          }
        }
      }
      checkMetro_in(met, Nil())
    }
  }
  
  val a: Metro = AREA("a", STATION("a"))
  val b: Metro = AREA("a", AREA("a", STATION("a")))
  val c: Metro = AREA("a", AREA("b", CONNECT(STATION("a"), STATION("b"))))
  val d: Metro = AREA("a", CONNECT(STATION("a"), AREA("b", STATION("a"))))
  
  val e: Metro = AREA("a", STATION("b"))
  val f: Metro = AREA("a", CONNECT(STATION("a"), AREA("b", STATION("c"))))
  val g: Metro = AREA("a", AREA("b", CONNECT(STATION("a"), STATION("c"))))
  val h: Metro = STATION("b")
}