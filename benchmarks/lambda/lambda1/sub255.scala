import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub255 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checker(name_list: List[Name], m: Metro): Boolean = {
    m match {
      case STATION(s) => {
        val _2 = {
          def p(str) = { s == str }
          name_list.exists(p)
        }
      }
      case AREA(a, m) => { checker(a :: name_list, m) }
      case CONNECT(m1, m2) => { checker(name_list, m1) && checker(name_list, m2)
      }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checker(Nil(), m) }
}