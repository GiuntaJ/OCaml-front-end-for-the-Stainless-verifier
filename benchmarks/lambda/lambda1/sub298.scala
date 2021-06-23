import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub298 {
  /*
      PL 2-3
      2008-11609 박성원
  */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def findInList(l, v) = {
        l match {
          case Nil() => { false }
          case Cons(h, l) => { h == v || findInList(l, v) }
        }
      }
      val _3 = {
        def checkImpl(metro, areaNames) = {
          metro match {
            case STATION(name) => { findInList(areaNames, name) }
            case AREA(name, metro) => { checkImpl(metro, name :: areaNames) }
            case CONNECT(metro1, metro2) => {
              checkImpl(metro1, areaNames) && checkImpl(metro2, areaNames)
            }
          }
        }
        checkImpl(metro, Nil())
      }
    }
  }
}