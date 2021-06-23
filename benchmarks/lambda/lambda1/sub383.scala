import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub383 {
  /*HW2-Exercise 4*/
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checkId(met, id_list) = {
        met match {
          case STATION(name) => { id_list.contains(name) }
          case AREA(name, met0) => { checkId(met0, name :: id_list) }
          case CONNECT(met1, met2) => {
            checkId(met1, id_list) && checkId(met2, id_list)
          }
        }
      }
      checkId(met, Nil())
    }
  }
}