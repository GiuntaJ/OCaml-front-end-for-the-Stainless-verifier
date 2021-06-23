import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub107 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def cm(nowl, mect) = {
        mect match {
          case STATION(a) => { nowl.contains(a) }
          case AREA(n, m) => { cm(n :: nowl, m) }
          case CONNECT(a, b) => { cm(nowl, a) && cm(nowl, b) }
        }
      }
      cm(Nil(), met)
    }
  }
  
}
