import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub11 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def make_area_list(m) = {
        m match {
          case STATION(n) => { Nil() }
          case AREA(n, m1) => { n :: make_area_list(m1) }
          case CONNECT(m1, m2) => { make_area_list(m1) ++ make_area_list(m2) }
        }
      }
      val _3 = {
        def match_list_with_station(m, l) = {
          m match {
            case AREA(n, m1) => { match_list_with_station(m1, l) }
            case CONNECT(m1, m2) => {
              match_list_with_station(m1, l) && match_list_with_station(m2, l)
            }
            case STATION(n) => {
              if (l.exists(( (x) => { x == n } ))) true else false
            }
          }
        }
        match_list_with_station(m, make_area_list(m))
      }
    }
  }
  	
  		
}