import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub417 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def checkmetro_0(m, namelist) = {
            m match {
              case STATION(name) => {
                if (namelist.contains(name)) true else false
              }
              case AREA(name, metro) => { checkmetro_0(metro, name :: namelist)
              }
              case CONNECT(met1, met2) => {
                checkmetro_0(met1, namelist) && checkmetro_0(met2, namelist)
              }
            }
          }
          checkmetro_0(m, Nil())
        }
    }
  )
  
}
