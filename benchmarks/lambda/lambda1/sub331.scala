import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub331 {
  /*2-3 컴공 2014-10618 이세영*/
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def checkList(li, met) = {
        met match {
          case STATION(a) => {
            val _5 = {
              def find = (
                x =>
                  x match {
                    case Nil() => { false }
                    case Cons(x, li) => { if (x == a) true else find(li) }
                  }
              )
              find(li)
            }
          }
          case AREA(x, y) => {
            if (checkList(x :: li, y) == true) true else false
          }
          case CONNECT(x, y) => {
            
              if (
                checkList(li, x) == true && checkList(li, y) == true
              ) {
                true 
              } else {
                false
              }
          }
        }
      }
      checkList(Nil(), met)
    }
  }
}