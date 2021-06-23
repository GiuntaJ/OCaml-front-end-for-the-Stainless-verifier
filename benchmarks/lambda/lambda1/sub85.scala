import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub85 {
  /* programming language, exercise07, 200611810 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met) = {
    met match {
      case STATION(n) => { true }
      case CONNECT(met1, met2) => { checkMetro(met1) && checkMetro(met2) }
      case AREA(stn0, met0) => {
        val _2 = {
          def check(((a, b))) = { if (a == b) true else false }
          val _3 = {
            def chstn(((stn, met))) = {
              met match {
                case STATION(stn_0) => { check(stn, stn_0) }
                case CONNECT(n1, n2) => { chstn(stn, n1) || chstn(stn, n2) }
                case AREA(st, n3) => { check(stn, st) || chstn(stn, n3) }
              }
            }
            chstn(stn0, met0)
          }
        }
      }
    }
  }
  
  		
  
  	
   
}