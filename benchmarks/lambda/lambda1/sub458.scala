import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub458 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /* checkMetro: metro -> bool */
  def checkMetro(met: Metro): Boolean = { aux(Nil(), met) }
  def aux(namelist: List[Name], met: Metro): Boolean = {
    met match {
      case STATION(n) => { contains(n, namelist) }
      case AREA(n, m1) => { aux(n :: namelist, m1) }
      case CONNECT(m1, m2) => { aux(namelist, m1) && aux(namelist, m2) }
    }
  }
  def contains(((n, namelist))) = {
    namelist match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (n == hd) true else contains(n, tl) }
    }
  }
  
  /* Testcases */
  /*
  let _ =
  let myprint met = if (checkMetro met) then print_int 1 else print_int 0 in
  /* true */
  myprint(AREA("a", STATION "a"));
  myprint(AREA("a", AREA("a", STATION "a")));
  myprint(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))));
  myprint(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))));
  
  /* false */
  myprint(AREA("a", STATION "b"));
  myprint(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))));
  myprint(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))
  ;;
  let _=
  let print_bool x = print_endline (string_of_bool x) in
  
  let a81 = checkMetro (AREA("a", STATION "a")) in
  let a82 = checkMetro (AREA("a", AREA("a", STATION "a"))) in
  let a83 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) in
  let a84 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) in
  let a85 = checkMetro (AREA("a", STATION "b")) in
  let a86 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) in
  let a87 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) in
  
  print_bool(false = checkMetro ( STATION "a"));
  print_bool(true = checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a"))))));
  print_bool(false = checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c"))))));
  print_bool(true = a81);
  print_bool(true = a82);
  print_bool(true = a83);
  print_bool(true = a84);
  print_bool(false = a85);
  print_bool(false = a86);
  print_bool(false = a87)
  ;;
  */
}