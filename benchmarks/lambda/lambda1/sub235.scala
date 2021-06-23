import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub235 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      val area_list = Nil()
      val _3 = {
        def check(mtr: Metro, al: List[Name]): Boolean = {
          mtr match {
            case STATION(n) => { al.contains(n) }
            case AREA(n, mtr1) => { check(mtr1, n :: al) }
            case CONNECT(mtr1, mtr2) => { check(mtr1, al) && check(mtr2, al) }
          }
        }
        check(m, area_list)
      }
    }
  }
}
