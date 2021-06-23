import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub174 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (mm) =>
      {
        val _4 = {
          def checkMetroL(mmm, l) = {
            mmm match {
              case STATION(name) => { l.contains(name) }
              case AREA(name, m) => { checkMetroL(m, l ++ List(name)) }
              case CONNECT(m1, m2) => { checkMetroL(m1, l) && checkMetroL(m2, l)
              }
            }
          }
          checkMetroL(mm, Nil())
        }
    }
  )
}