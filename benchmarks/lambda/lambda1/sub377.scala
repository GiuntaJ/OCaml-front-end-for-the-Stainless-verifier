import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub377 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (metro) =>
      {
        val _4 = {
          def checkArea: (List[String], Metro) => Boolean = {
            case (nl, m) =>
              {
                m match {
                  case STATION(s) => {
                    val _7 = {
                      def isExist: (List[String], String) => Boolean = {
                        case (str_list, str) =>
                          {
                            str_list match {
                              case Nil() => { false }
                              case Cons(e, e_list) => {
                                if (e == str) true else isExist(e_list, str)
                              }
                            }
                        }
                      }
                      isExist(nl, s)
                    }
                  }
                  case AREA(a, b) => { checkArea(a :: nl, b) }
                  case CONNECT(a, b) => { checkArea(nl, a) && checkArea(nl, b) }
                }
            }
          }
          metro match {
            case STATION(n) => { false }
            case AREA(n, m) => { checkArea(List(n), m) }
            case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
          }
        }
    }
  )
}