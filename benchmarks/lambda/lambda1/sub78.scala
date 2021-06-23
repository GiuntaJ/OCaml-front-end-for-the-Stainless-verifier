import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub78 {
  /* 2009-13384, CHO Hyunik */
  
  
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def correctMetro(lst, met) = {
        met match {
          case AREA(a, b) => { correctMetro(a :: lst, b) }
          case CONNECT(a, b) => { correctMetro(lst, a) && correctMetro(lst, b) }
          case STATION(a) => { lst.contains(a) }
        }
      }
      correctMetro(Nil(), met)
    }
  }
}