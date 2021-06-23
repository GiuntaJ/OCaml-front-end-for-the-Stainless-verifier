import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub437 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroHelper: (List[Name], Metro) => Boolean = {
    case (env, inputMetro) =>
      {
        inputMetro match {
          case STATION(a) => { if (env.contains(a)) true else false }
          case CONNECT(a, b) => {
            checkMetroHelper(env, a) && checkMetroHelper(env, b)
          }
          case AREA(a, b) => {
            val _2 = {
              val newEnv = a :: env
              checkMetroHelper(newEnv, b)
            }
          }
        }
    }
  }
  
  def checkMetro: Metro => Boolean = ( (x) => { checkMetroHelper(Nil(), x) } )
}