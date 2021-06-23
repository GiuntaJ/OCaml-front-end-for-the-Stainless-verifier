import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub326 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def checkMetro(map: Metro): Boolean = {
    val _2 = {
      def checkMetro_0(((arealist, map))) = {
        map match {
          case STATION(name) => { if (arealist.contains(name)) true else false }
          case CONNECT(map1, map2) => {
            checkMetro_0(arealist, map1) && checkMetro_0(arealist, map2)
          }
          case AREA(name, map_0) => { checkMetro_0(name :: arealist, map_0) }
        }
      }
      checkMetro_0(Nil(), map)
    }
  }
}