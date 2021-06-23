import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub202 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def sset(e, set) = {
        set match {
          case Nil() => { false }
          case Cons(hd, tl) => { e == hd || sset(e, tl) }
        }
      }
      val _3 = {
        def inarray(f, lst) = {
          f match {
            case STATION(n) => { sset(n, lst) }
            case AREA(n, m) => { inarray(m, n :: lst) }
            case CONNECT(m1, m2) => { inarray(m1, lst) && inarray(m2, lst) }
          }
        }
        inarray(met, Nil())
      }
    }
  } 
}