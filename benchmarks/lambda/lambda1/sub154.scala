import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub154 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def checkMetroSub(metro, name_list) = {
        metro match {
          case AREA(name, m) => { checkMetroSub(m, name_list ++ List(name)) }
          case CONNECT(m1, m2) => {
            checkMetroSub(m1, name_list) && checkMetroSub(m2, name_list)
          }
          case STATION(name) => { name_list.contains(name) }
        }
      }
      checkMetroSub(metro, Nil())
    }
  }
}