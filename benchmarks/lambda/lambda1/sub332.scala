import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub332 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(input: Metro): Boolean = {
    val _2 = {
      def aux(arealist) = {
        (
          x =>
            x match {
              case STATION(m) => { arealist.contains(m) }
              case AREA(n, m) => { aux(n :: arealist, m) }
              case CONNECT(m1, m2) => { aux(arealist, m1) && aux(arealist, m2) }
            }
        )
      }
      aux(Nil(), input)
    }
  }
}