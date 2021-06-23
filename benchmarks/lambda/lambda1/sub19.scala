import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub19 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(a: Metro): Boolean = {
    val _2 = {
      def cm(l, a) = {
        a match {
          case STATION(name) => { l.contains(name) }
          case AREA(name, metro) => { cm(l ++(List(name)), metro) }
          case CONNECT(metro, metro2) => { cm(l, metro) && cm(l, metro2) }
        }
      }
      cm(Nil(), a)
    }
  }
}