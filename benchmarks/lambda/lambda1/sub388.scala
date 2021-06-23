import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub388 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def containCheck(((n, ll))): Boolean = {
    ll match {
      case Cons(hd, tl) => { if (hd == n) true else containCheck(n, tl) }
      case Nil() => { false }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def check(((metro, ll))): Boolean = {
        metro match {
          case STATION(n) => { containCheck(n, ll) }
          case AREA(n, met) => { check(met, n :: ll) }
          case CONNECT(met1, met2) => { check(met1, ll) && check(met2, ll) }
        }
      }
      check(metro, Nil())
    }
  }
}