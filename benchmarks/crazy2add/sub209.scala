import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub209 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /* crazy2add: crazy2 * crazy2 -> crazy2 */
  def crazy2add(((x, y))) = {
    x match {
      case NIL => { y }
      case ZERO(a) => {
        y match {
          case NIL => { x }
          case ZERO(b) => { ZERO(crazy2add(a, b)) }
          case ONE(b) => { ONE(crazy2add(a, b)) }
          case MONE(b) => { MONE(crazy2add(a, b)) }
        }
      }
      case ONE(a) => {
        y match {
          case NIL => { x }
          case ZERO(b) => { ONE(crazy2add(a, b)) }
          case ONE(b) => { ZERO(crazy2add(ONE(NIL), crazy2add(a, b))) }
          case MONE(b) => { ZERO(crazy2add(a, b)) }
        }
      }
      case MONE(a) => {
        y match {
          case NIL => { x }
          case ZERO(b) => { MONE(crazy2add(a, b)) }
          case ONE(b) => { ZERO(crazy2add(a, b)) }
          case MONE(b) => { ZERO(crazy2add(MONE(NIL), crazy2add(a, b))) }
        }
      }
    }
  }
  
  /* Testcases */
  /*
  let rec crazy2val: crazy2 -> int = fun x ->
    match x with
    | NIL -> 0
    | ZERO y -> 2 * crazy2val(y)
    | ONE y -> 1 + 2 * crazy2val(y)
    | MONE y -> -1 + 2 * crazy2val(y)
  ;;
  
  let _=
  let print_bool x = print_endline (string_of_bool x) in
  
  print_bool (0 = (crazy2val (crazy2add (ZERO NIL, ZERO NIL))));
  print_bool (0 = (crazy2val (crazy2add (MONE NIL, ONE NIL))));
  print_bool (1 = (crazy2val (crazy2add (ZERO NIL, ONE NIL))));
  print_bool (4 = (crazy2val (crazy2add (ONE (ONE NIL), ONE NIL))));
  print_bool (-683 = (crazy2val (crazy2add (MONE (ZERO (ZERO (ZERO NIL))), (ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL)))))))))))))))
  ;;
  */
}