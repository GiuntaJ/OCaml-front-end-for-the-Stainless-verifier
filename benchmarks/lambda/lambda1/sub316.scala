import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub316 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def metroWithList(metro, mlist) = {
        metro match {
          case STATION(n) => { mlist.contains(n) }
          case AREA(n, m) => { metroWithList(m, List(n) ++ mlist) }
          case CONNECT(m1, m2) => {
            metroWithList(m1, mlist) && metroWithList(m2, mlist)
          }
        }
      }
      metroWithList(metro, Nil())
    }
  }
}
