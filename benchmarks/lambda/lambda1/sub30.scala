import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub30 {
  /* Exercise 7 */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkId(((metro, env))) = {
    metro match {
      case STATION(id) => { env.exists(( (x) => { x == id } )) }
      case AREA(id, submetro) => { checkId(submetro, id :: env) }
      case CONNECT(submetro1, submetro2) => {
        checkId(submetro1, env) && checkId(submetro2, env)
      }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = { checkId(metro, Nil()) }
}