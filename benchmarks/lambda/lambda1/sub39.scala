import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub39 {
  type Name = String
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def checkMetro(metr: Metro): Boolean = {
    val _2 = {
      def listUnion(a, b) = {
        a match {
          case Cons(h, t) => { if (b.contains(h)) b else h :: b }
          case Nil() => { b }
        }
      }
      val _3 = {
        def subCheckMetro(set, met) = {
          met match {
            case STATION(nam) => { set.contains(nam) }
            case AREA(nam, me) => { subCheckMetro(listUnion(List(nam), set), me)
            }
            case CONNECT(me1, me2) => {
              subCheckMetro(set, me1) && subCheckMetro(set, me2)
            }
          }
        }
        subCheckMetro(Nil(), metr)
      }
    }
  }
}
