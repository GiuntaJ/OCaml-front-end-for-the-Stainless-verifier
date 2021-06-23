import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub96 {
  /* ex8 */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checker(((met, lst))) = {
        met match {
          case AREA(str, met_) => { checker(met_, str :: lst) }
          case CONNECT(met1, met2) => { checker(met1, lst) && checker(met2, lst)
          }
          case STATION(str) => { lst.contains(str) }
        }
      }
      checker(m, Nil())
    }
  }
}