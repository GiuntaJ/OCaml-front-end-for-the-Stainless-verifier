import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub103 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def proc(((m, met_list))) = {
        m match {
          case STATION(name) => { met_list.contains(name) }
          case AREA(name, metro) => { proc(metro, name :: met_list) }
          case CONNECT(a, b) => { proc(a, met_list) && proc(b, met_list) }
        }
      }
      proc(met, Nil())
    }
  }
  
    
}