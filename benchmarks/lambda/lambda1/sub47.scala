import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub47 {
  sealed case class Error(param0: String) extends Exception {}
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkMetro_in(m, areas) = {
        m match {
          case STATION(st) => {
            val _5 = {
              def is_valid(st, area_list) = {
                area_list match {
                  case Nil() => { false }
                  case Cons(h, t) => { if (h == st) true else is_valid(st, t) }
                }
              }
              is_valid(st, areas)
            }
          }
          case AREA(st, m_0) => { checkMetro_in(m_0, st :: areas) }
          case CONNECT(m1, m2) => {
            checkMetro_in(m1, areas) && checkMetro_in(m2, areas)
          }
        }
      }
      checkMetro_in(m, Nil())
    }
  }	
}