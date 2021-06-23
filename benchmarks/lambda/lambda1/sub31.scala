import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub31 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String 
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetro_sub(m1, li) = {
        m1 match {
          case STATION(a) => { li.contains(a) }
          case AREA(name, met) => { checkMetro_sub(met, li ++(List(name))) }
          case CONNECT(met1, met2) => {
            checkMetro_sub(met1, li) && checkMetro_sub(met2, li)
          }
        }
      }
      checkMetro_sub(m, Nil())
    }
  } 
  
  	
}