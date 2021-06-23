import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub278 {
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def checkMetroInternal: (List[String], Metro) => Boolean = {
            case (sl, m) =>
              {
                m match {
                  case STATION(n) => { sl.contains(n) }
                  case AREA(n, m) => { checkMetroInternal(n :: sl, m) }
                  case CONNECT(m1, m2) => {
                    checkMetroInternal(sl, m1) && checkMetroInternal(sl, m2)
                  }
                }
            }
          }
          checkMetroInternal(Nil(), m)
        }
    }
  )
  
  
  /*
  
  let t1 = AREA("a", STATION "a")
  let t2 = AREA("a", AREA("a", STATION "a"))
  let t3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
  let t4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
  
  let f1 = AREA("a", STATION "b")
  let f2 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
  let f3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
  
  let _ = print_endline(string_of_bool(checkMetro(t1)))
  let _ = print_endline(string_of_bool(checkMetro(t2)))
  let _ = print_endline(string_of_bool(checkMetro(t3)))
  let _ = print_endline(string_of_bool(checkMetro(t4)))
  let _ = print_endline(string_of_bool(checkMetro(f1)))
  let _ = print_endline(string_of_bool(checkMetro(f2)))
  let _ = print_endline(string_of_bool(checkMetro(f3)))
  */
}