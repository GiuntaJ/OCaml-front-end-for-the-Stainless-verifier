import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub110 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkElementInList(((elem, lst))) = {
        lst match {
          case Cons(hd, tl) => {
            if (elem == hd) true else checkElementInList(elem, tl)
          }
          case Nil() => { false }
        }
      }
      val _3 = {
        def subset(inner_list, outer_list) = {
          inner_list match {
            case Cons(hd, tl) => {
              
                if (
                  checkElementInList(hd, outer_list)
                ) {
                  subset(tl, outer_list) 
                } else {
                  false
                }
            }
            case Nil() => { true }
          }
        }
        val _4 = {
          def makeAreaList(m_eq, a_lst) = {
            m_eq match {
              case STATION(x) => { a_lst }
              case AREA(x, y) => { makeAreaList(y, x :: a_lst) }
              case CONNECT(x, y) => {
                makeAreaList(x, a_lst) ++ makeAreaList(y, Nil())
              }
            }
          }
          val _5 = {
            def makeStationList(m_eq, s_lst) = {
              m_eq match {
                case STATION(x) => { x :: s_lst }
                case AREA(x, y) => { makeStationList(y, s_lst) }
                case CONNECT(x, y) => {
                  makeStationList(x, s_lst) ++ makeStationList(y, Nil())
                }
              }
            }
            subset(makeStationList(m, Nil()), makeAreaList(m, Nil()))
          }
        }
      }
    }
  }
}
