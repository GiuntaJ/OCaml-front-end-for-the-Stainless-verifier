import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub142 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def rec_check(m, lst) = {
        m match {
          case STATION(name) => { lst.contains(name) }
          case AREA(name, m1) => { rec_check(m1, name :: lst) }
          case CONNECT(m1, m2) => { rec_check(m1, lst) && rec_check(m2, lst) }
        }
      }
      rec_check(metro, Nil())
    }
  }
         
}