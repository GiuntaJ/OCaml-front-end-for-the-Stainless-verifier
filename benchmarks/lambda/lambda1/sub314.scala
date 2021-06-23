import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub314 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (x) =>
      {
        val _4 = {
          def listing(m, l) = {
            m match {
              case STATION(st) => { l.contains(st) }
              case AREA(aname, mm) => { listing(mm, aname :: l) }
              case CONNECT(m1, m2) => { listing(m1, l) && listing(m2, l) }
            }
          }
          listing(x, Nil())
        }
    }
  )
  
  val x1: Metro = AREA("a", STATION("a"))
  val x2: Metro = AREA("a", AREA("a", STATION("a")))
  val x3: Metro = AREA("a", AREA("b", CONNECT(STATION("a"), STATION("b"))))
  val x4: Metro = AREA("a", CONNECT(STATION("a"), AREA("b", STATION("a"))))
  val x5: Metro = AREA("a", STATION("b"))
  val x6: Metro = AREA("a", CONNECT(STATION("a"), AREA("b", STATION("c"))))
  val x7: Metro = AREA("a", AREA("b", CONNECT(STATION("a"), STATION("c"))))
}