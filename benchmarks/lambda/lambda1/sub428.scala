import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub428 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (metro) =>
      {
        val _4 = {
          def checkhelper(met, nlist) = {
            met match {
              case AREA(id, m) => { checkhelper(m, id :: nlist) }
              case STATION(n) => { if (nlist.contains(n)) true else false }
              case CONNECT(m1, m2) => {
                checkhelper(m1, nlist) && checkhelper(m2, nlist)
              }
            }
          }
          checkhelper(metro, Nil())
        }
    }
  )
}