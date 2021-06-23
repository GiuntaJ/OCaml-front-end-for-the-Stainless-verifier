import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub239 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = val _0 = {
    def checkMetror: (Metro, List[Name]) => Boolean = {
      case (code, vars) =>
        {
          code match {
            case STATION(var0) => { vars.contains(var0) }
            case CONNECT(code1, code2) => {
              checkMetror(code1, vars) && checkMetror(code2, vars)
            }
            case AREA(new_var, code_in) => {
              checkMetror(code_in, new_var :: vars)
            }
          }
      }
    }
    ( (code) => { checkMetror(code, Nil()) } )
  }
}