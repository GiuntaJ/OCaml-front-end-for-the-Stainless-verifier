import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub207 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def cm(((arlist, met))) = {
        met match {
          case STATION(name) => { arlist.contains(name) }
          case AREA(aname, m) => { cm(aname :: arlist, m) }
          case CONNECT(m1, m2) => { cm(arlist, m1) && cm(arlist, m2) }
        }
      }
      metro match {
        case STATION(_) | CONNECT(_) => { cm(Nil(), metro) }
        case AREA(aname, m) => { cm(List(aname), m) }
      }
    }
  }
}