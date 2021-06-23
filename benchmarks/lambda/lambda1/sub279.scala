import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub279 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro_withList(m: Metro, l: List[Name]): Boolean = {
    m match {
      case STATION(name) => { l.contains(name) }
      case AREA(name, metro) => { checkMetro_withList(metro, name :: l) }
      case CONNECT(metro1, metro2) => {
        checkMetro_withList(metro1, l) && checkMetro_withList(metro2, l)
      }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetro_withList(m, Nil()) }
  
  /* using test */
  /*
  let _ =
  	let msg = string_of_bool (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", AREA("c", STATION "c")))))) in
  	print_endline msg
  
  let _ =
  	let msg = string_of_bool (checkMetro (AREA("a", STATION "b"))) in
  	print_endline msg
  
  let _ =
  	let msg = string_of_bool (checkMetro (AREA("a", AREA("a", STATION "a")))) in
  	print_endline msg
  */
}