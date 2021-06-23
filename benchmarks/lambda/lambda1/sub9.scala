import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub9 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def append(m1, m2) = {
        m1 match {
          case Cons(h, t) => {
            if (m2.contains(h)) append(t, m2) else append(t, h :: m2)
          }
          case Nil() => { m2 }
        }
      }
      val _3 = {
        def checkMetro2(m, l) = {
          m match {
            case STATION(n) => { l }
            case AREA(n, m2) => {
              if (l.contains(n)) checkMetro2(m2, l) else checkMetro2(m2, n :: l)
            }
            case CONNECT(m1, m2) => {
              append(checkMetro2(m1, l), checkMetro2(m2, l))
            }
          }
        }
        val _4 = {
          def checkMetro3(m, l) = {
            m match {
              case STATION(n) => { if (l.contains(n)) true else false }
              case AREA(n, m2) => { checkMetro3(m2, l) }
              case CONNECT(m1, m2) => { checkMetro3(m1, l) && checkMetro3(m2, l)
              }
            }
          }
          val _5 = {
            val l = checkMetro2(m, Nil())
            checkMetro3(m, l)
          }
        }
      }
    }
  }		
}
