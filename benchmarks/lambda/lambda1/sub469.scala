import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub469 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def list_finder: (List[A], A) => Boolean = {
    case (list_, element_) =>
      {
        list_ match {
          case Nil() => { false }
          case Cons(hd, tail) => {
            if (hd == element_) true else list_finder(tail, element_)
          }
        }
    }
  }
  
  def checkMetro_helper: (Metro, List[String]) => Boolean = {
    case (met, env) =>
      {
        met match {
          case STATION(st_name) => { list_finder(env, st_name) }
          case AREA(ar_name, metro_) => {
            checkMetro_helper(metro_, ar_name :: env)
          }
          case CONNECT(metro1_, metro2_) => {
            checkMetro_helper(metro1_, env) && checkMetro_helper(metro2_, env)
          }
        }
    }
  }
  
  def checkMetro: Metro => Boolean = ( (met) => { checkMetro_helper(met, Nil()) } )
  
  
}
