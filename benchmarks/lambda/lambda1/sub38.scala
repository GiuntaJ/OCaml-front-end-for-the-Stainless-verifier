import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub38 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def is_member(e, l) = {
        l match {
          case Nil() => { false }
          case Cons(h, t) => { e == h || is_member(e, t) }
        }
      }
      val _3 = {
        def check(mtr, alst) = {
          mtr match {
            case STATION(name) => { is_member(name, alst) }
            case AREA(name, m) => { check(m, name :: alst) }
            case CONNECT(m1, m2) => { check(m1, alst) && check(m2, alst) }
          }
        }
        check(metro, Nil())
      }
    }
  }
}