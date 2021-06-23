import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub363 {
  /* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-4 */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def helper: (Metro, List[Name]) => Boolean = {
    case (a, l) =>
      {
        a match {
          case STATION(p) => { l.contains(p) }
          case CONNECT(s, t) => { helper(s, l) && helper(t, l) }
          case AREA(p, s) => { helper(s, p :: l) }
        }
    }
  }
  
  val checkMetro: Metro => Boolean = ( (a) => { helper(a, Nil()) } )
  
  /* Test Code 
  let x : metro = AREA("a", STATION "a")
  let y : metro = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
  let z : metro = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
  let w : metro = AREA("a", CONNECT(STATION "a", AREA("b", STATION "b")))
  let t : metro = AREA("a", CONNECT(AREA("b", STATION "a"), AREA("c", STATION "c")))
  let u : metro = AREA("c", AREA("a", CONNECT(STATION "a", STATION "a")))
  let x' : metro = STATION "a"
  let y' : metro = CONNECT(AREA("a", STATION "a"), STATION("a"))
  let z' : metro = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
  let w' : metro = AREA("b", CONNECT(AREA("a", STATION "a"), STATION "a"))
  let t' : metro = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
  let u' : metro = AREA("a", CONNECT(STATION "b", AREA("b", STATION "a")))
  
  let test m = match checkMetro(m) with
      | true -> print_endline("true")
      | false -> print_endline("false")
  
  let _ = test(x)
  let _ = test(y)
  let _ = test(z)
  let _ = test(w)
  let _ = test(t)
  let _ = test(u)
  let _ = test(AREA("c",CONNECT(x, w)))
  let _ = test(AREA("c", t'))
  let _ = print_newline ()
  let _ = test(x')
  let _ = test(y')
  let _ = test(z')
  let _ = test(w')
  let _ = test(t')
  let _ = test(u')
  */
}