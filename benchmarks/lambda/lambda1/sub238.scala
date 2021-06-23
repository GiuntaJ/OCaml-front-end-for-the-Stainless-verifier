import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub238 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (met) =>
      {
        val _4 = {
          def stationname: Metro => List[Name] = (
            (f) =>
              {
                f match {
                  case AREA(a, b) => { stationname(b) }
                  case STATION(a) => { List(a) }
                  case CONNECT(a, b) => { stationname(a) ++(stationname(b)) }
                }
            }
          )
          val _5 = {
            def areaname: Metro => List[Name] = (
              (f) =>
                {
                  f match {
                    case AREA(a, b) => { List(a) ++(areaname(b)) }
                    case CONNECT(a, b) => { areaname(a) ++(areaname(b)) }
                    case STATION(a) => { Nil() }
                  }
              }
            )
            val _6 = {
              def haveit: (List[Name], List[Name]) => Boolean = {
                case (a, b) =>
                  {
                    val _9 = {
                      val predicate: Name => Boolean = ( (f) => { b.contains(f) } )
                      a.forall(predicate)
                    }
                }
              }
              met match {
                case AREA(a, b) => { haveit(stationname(b), areaname(met)) }
                case CONNECT(a, b) => { checkMetro(a) && checkMetro(b) }
                case STATION(a) => { false }
              }
            }
          }
        }
    }
  )
  
}
