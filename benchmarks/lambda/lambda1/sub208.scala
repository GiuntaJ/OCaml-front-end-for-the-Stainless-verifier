import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub208 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def exists(((l, a))) = {
    l match {
      case Nil() => { false }
      case Cons(h, t) => { exists(t, a) || a == h }
    }
  }
  
  def _checkMetro(a: List[Name], metro: Metro): Boolean = {
    metro match {
      case STATION(name) => { exists(a, name) }
      case AREA(name, metro) => { _checkMetro(name :: a, metro) }
      case CONNECT(metro1, metro2) => {
        _checkMetro(a, metro1) && _checkMetro(a, metro2)
      }
    }
  }
  
  val checkMetro: Metro => Boolean = ( (a) => { _checkMetro(Nil(), a) } )
}