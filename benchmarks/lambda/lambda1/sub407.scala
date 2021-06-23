import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub407 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (met) =>
      {
        val _4 = {
          def metroList(stList, mtr) = {
            mtr match {
              case STATION(name) => { stList.contains(name) }
              case AREA(name, metro) => { metroList(name :: stList, metro) }
              case CONNECT(met1, met2) => {
                metroList(stList, met1) && metroList(stList, met2)
              }
            }
          }
          metroList(Nil(), met)
        }
    }
  )
  
}
