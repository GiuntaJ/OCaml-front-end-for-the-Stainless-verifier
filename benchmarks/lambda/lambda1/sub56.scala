import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub56 {
  /* 2004-11951 Noh, Soon Hyun */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /* metrolist -> bool */
  /* true if correct, false if incorrect */
  def protoCheck(areaList: List[Name], met: Metro): Boolean = {
    (areaList, met) match {
      case (Nil(), STATION(stationName)) => { false }
      case (Cons(a, remainList), STATION(stationName)) => {
        (if (a == stationName) true else false) || protoCheck(remainList, met)
      }
      case (_, AREA(areaName, met1)) => { protoCheck(areaName :: areaList, met1)
      }
      case (_, CONNECT(met1, met2)) => {
        protoCheck(areaList, met1) && protoCheck(areaList, met2)
      }
    }
  }
  
  def checkMetro(met: Metro): Boolean = { protoCheck(Nil(), met) }
  
  /* Test Code :: 
  let print_bool a =
          if a=true then print_string "true
  "
          else print_string "false
  "
  
  let _ = print_bool (checkMetro (AREA("a", STATION "a"))); print_char '
  '
  let _ = print_bool (checkMetro (AREA("a", AREA("a", STATION "a")))); print_char '
  '
  let _ = print_bool (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))); print_char '
  '
  let _ = print_bool (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))); print_char '
  '
  
  let _ = print_bool (checkMetro (AREA("a", STATION "b"))); print_char '
  '
  let _ = print_bool (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))); print_char '
  '
  let _ = print_bool (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))); print_char '
  '
  */
}