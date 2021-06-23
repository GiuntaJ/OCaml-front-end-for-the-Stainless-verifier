import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub402 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def checkArea(((lst, n))) = {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == n) true else checkArea(tl, n) }
        }
      }
      val _3 = {
        def add_env(((env, n))) = {
          env match {
            case Nil() => { List(n) }
            case Cons(hd, tl) => { if (hd == n) env else hd :: add_env(tl, n) }
          }
        }
        val _4 = {
          def my_check(((env, m))) = {
            m match {
              case STATION(n) => { checkArea(env, n) }
              case AREA(n, m) => { my_check(add_env(env, n), m) }
              case CONNECT(m1, m2) => { my_check(env, m1) && my_check(env, m2) }
            }
          }
          my_check(Nil(), m)
        }
      }
    }
  }
}
