import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub95 {
  /* 2008-11874 EXERCISE 8 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def nameStation(metro) = {
        metro match {
          case STATION(n) => { List(n) }
          case AREA(n, m) => { nameStation(m) }
          case CONNECT(m1, m2) => { nameStation(m1) ++(nameStation(m2)) }
        }
      }
      val _3 = {
        def nameArea(metro) = {
          metro match {
            case STATION(n) => { Nil() }
            case AREA(n, m) => { n :: nameArea(m) }
            case CONNECT(m1, m2) => { nameArea(m1) ++(nameArea(m2)) }
          }
        }
        val _4 = {
          def check(l1, l2) = {
            l1 match {
              case Nil() => { true }
              case Cons(hd, tl) => { l2.contains(hd) && check(tl, l2) }
            }
          }
          check(nameStation(metro), nameArea(metro))
        }
      }
    }
  }
  	
  	
}