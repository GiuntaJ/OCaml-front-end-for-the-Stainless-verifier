import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub410 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def check(id) = {
    (
      x =>
        x match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == id) true else check(id, tl) }
        }
    )
  }
  
  def foo(areas) = {
    (
      x =>
        x match {
          case STATION(id) => { check(id, areas) }
          case AREA(id, m) => { foo(List(id) ++ areas, m) }
          case CONNECT(m1, m2) => { foo(areas, m1) && foo(areas, m2) }
        }
    )
  }
  
  def checkMetro: Metro => Boolean = foo(Nil())
  
  /*
  let _ =
    let test_case : int * bool -> unit = fun (n, x) ->
      print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in
    test_case(1, true == checkMetro(AREA("a", STATION "a")));
    test_case(2, true == checkMetro(AREA("a", AREA("a", STATION "a"))));
    test_case(3, true == checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))));
    test_case(4, true == checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))));
    test_case(5, false == checkMetro(AREA("a", STATION "b")));
    test_case(6, false == checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))));
    test_case(7, false == checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))));
    test_case(8, true == checkMetro(CONNECT(AREA("a", STATION "a"), AREA("b", AREA("a", CONNECT(STATION "b", STATION "a"))))));
    test_case(9, false == checkMetro(CONNECT(AREA("c", STATION "c"), AREA("b", AREA("a", CONNECT(STATION "b", STATION "c"))))));
    test_case(10, false == checkMetro(STATION "a"))
    */
}