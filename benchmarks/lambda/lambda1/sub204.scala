import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub204 {
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (metro) =>
      {
        val _4 = {
          def checkMetroInner(metro, metroList) = {
            metro match {
              case STATION(name) => { metroList.contains(name) }
              case AREA(name, m) => { checkMetroInner(m, name :: metroList) }
              case CONNECT(m1, m2) => {
                checkMetroInner(m1, metroList) && checkMetroInner(m2, metroList)
              }
            }
          }
          checkMetroInner(metro, Nil())
        }
    }
  )
}