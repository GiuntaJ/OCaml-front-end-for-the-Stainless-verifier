import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub339 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def inList(lst, a) = {
        lst match {
          case Nil() => { false }
          case Cons(b, sublist) => { if (a == b) true else inList(sublist, a) }
        }
      }
      val _3 = {
        def aux(listArea, met) = {
          met match {
            case STATION(name) => { inList(listArea, name) }
            case AREA(name, submet) => { aux(name :: listArea, submet) }
            case CONNECT(submet1, submet2) => {
              aux(listArea, submet1) && aux(listArea, submet2)
            }
          }
        }
        aux(Nil(), met)
      }
    }
  }
}