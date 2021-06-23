import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub24 {
  sealed case class Error(param0: String) extends Exception {}
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def env_checker: (Metro, List[String]) => Boolean = {
    case (mymet, myls) =>
      {
        mymet match {
          case STATION(sta) => { if (myls.contains(sta)) true else false }
          case AREA(thename, themet) => {
            
              if (
                myls.contains(thename)
              ) {
                env_checker(themet, myls) 
              } else {
                env_checker(themet, thename :: myls)
              }
          }
          case CONNECT(bm, cm) => {
            env_checker(bm, myls) && env_checker(cm, myls)
          }
        }
    }
  }
  
  def checkMetro(alp: Metro): Boolean = { env_checker(alp, Nil()) }
}