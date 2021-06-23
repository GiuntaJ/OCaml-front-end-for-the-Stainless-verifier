import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub116 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def real_cM(metro, lst) = {
        (metro, lst) match {
          case (STATION(s), l) => { l.contains(s) }
          case (AREA(name, met), l) => { real_cM(met, l ++ List(name)) }
          case (CONNECT(met1, met2), l) => {
            real_cM(met1, l) && real_cM(met2, l)
          }
        }
      }
      real_cM(metro, Nil())
    }
  }
}