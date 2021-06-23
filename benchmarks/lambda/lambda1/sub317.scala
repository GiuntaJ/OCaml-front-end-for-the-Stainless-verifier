import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub317 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(input: Metro): Boolean = {
    val _2 = {
      def check(input: Metro, rules: List[String]): Boolean = {
        input match {
          case STATION(name) => { rules.exists(( (x) => { x == name } )) }
          case AREA(name, m0) => { check(m0, name :: rules) }
          case CONNECT(m0, m1) => { check(m0, rules) && check(m1, rules) }
        }
      }
      check(input, Nil())
    }
  }
}