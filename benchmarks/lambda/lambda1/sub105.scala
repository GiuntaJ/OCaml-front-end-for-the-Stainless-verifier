import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub105 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def check(m, l) = {
        m match {
          case STATION(a) => { l.contains(a) }
          case AREA(name, metro) => { check(metro, name :: l) }
          case CONNECT(m1, m2) => { check(m1, l) && check(m2, l) }
        }
      }
      check(m, Nil())
    }
  }
  
    
    /*
  let a = AREA("a", STATION "a")
  let b = AREA("a", AREA("a", STATION "a"))
  let c = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
  let d = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
  let e = AREA("a", STATION "b")
  let f = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
  let g = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
  
  let l = [a;b;c;d;e;f;g]
  
  let print_bool a =
    match a with
    | true -> print_endline "TRUE"
    | false -> print_endline "FALSE"
  
  let _ = List.iter print_bool (List.map checkMetro l)
  
  */
}