import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub370 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (a) =>
      {
        val _4 = {
          def checkMetroList: (List[Name], Metro) => Boolean = {
            case (l, m) =>
              {
                val _7 = {
                  def checkNameinList: (Name, List[Name]) => Boolean = {
                    case (n, nl) =>
                      {
                        nl match {
                          case Nil() => { false }
                          case Cons(nlhd, nltl) => {
                            n == nlhd || checkNameinList(n, nltl)
                          }
                        }
                    }
                  }
                  m match {
                    case STATION(n) => { checkNameinList(n, l) }
                    case AREA(n, mt) => { checkMetroList(n :: l, mt) }
                    case CONNECT(ma, mb) => {
                      checkMetroList(l, ma) && checkMetroList(l, mb)
                    }
                  }
                }
            }
          }
          checkMetroList(Nil(), a)
        }
    }
  )
}