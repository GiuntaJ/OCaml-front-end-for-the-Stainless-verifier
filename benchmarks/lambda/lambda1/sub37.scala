import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub37 {
  /* CSE/ 2004-11920 / Yeseong Kim/ Prob 7*/
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def subMetro(myM, l) = {
        myM match {
          case STATION(n) => { l.contains(n) }
          case AREA(n, subm) => { subMetro(subm, n :: l) }
          case CONNECT(m1, m2) => { subMetro(m1, l) && subMetro(m2, l) }
        }
      }
      subMetro(m, Nil())
    }
  }
}