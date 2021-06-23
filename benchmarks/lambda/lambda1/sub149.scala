import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub149 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def is_connect(m) = {
        m match {
          case STATION(_) => { false }
          case CONNECT(_, _) => { true }
          case AREA(name, metro) => { is_connect(metro) }
        }
      }
      val _3 = {
        def get_left(m) = {
          m match {
            case STATION(_) => { m }
            case CONNECT(l, _) => { l }
            case AREA(name, metro) => { AREA(name, get_left(metro)) }
          }
        }
        val _4 = {
          def get_right(m) = {
            m match {
              case STATION(_) => { m }
              case CONNECT(_, r) => { r }
              case AREA(name, metro) => { AREA(name, get_right(metro)) }
            }
          }
          
            if (
              is_connect(m)
            ) {
              checkMetro(get_left(m)) && checkMetro(get_right(m)) 
            } else {
              m match {
                case STATION(_) => { false }
                case CONNECT(l, r) => { checkMetro(l) && checkMetro(r) }
                case AREA(name, metro) => {
                  metro match {
                    case STATION(s) => { if (name == s) true else false }
                    case CONNECT(left, right) => {
                      checkMetro(AREA(name, left)) &&
                      checkMetro(AREA(name, right))
                    }
                    case AREA(name2, metro2) => {
                      checkMetro(AREA(name, metro2)) ||
                      checkMetro(AREA(name2, metro2))
                    }
                  }
                }
              }
            }
        }
      }
    }
  }
  
  
  
  
  
  
  
  
  
}
