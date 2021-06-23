import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub380 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def helper(m, area_li) = {
        m match {
          case STATION(n) => { area_li.exists(( (x) => { x == n } )) }
          case AREA(n, m) => { helper(m, n :: area_li) }
          case CONNECT(m1, m2) => { helper(m1, area_li) && helper(m2, area_li) }
        }
      }
      helper(m, Nil())
    }
  } 
}