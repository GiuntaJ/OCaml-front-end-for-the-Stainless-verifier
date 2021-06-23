import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub28 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(t: Metro): Boolean = {
    val _2 = {
      def getid(t, r) = {
        t match {
          case AREA(t1, t2) => { getid(t2, t1 :: r) }
          case CONNECT(t1, t2) => { getid(t1, r) && getid(t2, r) }
          case STATION(t1) => {
            r.exists(( (x) => { if (x == t1) true else false } ))
          }
        }
      }
      getid(t, Nil())
    }
  }
}