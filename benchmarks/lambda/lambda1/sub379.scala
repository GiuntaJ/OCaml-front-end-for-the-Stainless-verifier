import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub379 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def stationinList: (Metro, List[String]) => Boolean = (
    (c) =>
      {
        c match {
          case (STATION(a), b) => { b.contains(a) }
          case (AREA(a, b), c) => { stationinList(b, a :: c) }
          case (CONNECT(a, b), c) => {
            stationinList(a, c) && stationinList(b, c)
          }
        }
    }
  )
      
  def checkMetro: Metro => Boolean = (
    (c) =>
      {
        c match {
          case a => { stationinList(a, Nil()) }
        }
    }
  )
}