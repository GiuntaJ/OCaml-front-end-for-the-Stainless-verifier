import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub107 {
  /* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-3 */
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /*
  let rec crazy2val: crazy2 -> int = fun (a) ->
      match a with
      | NIL -> 0
      | ZERO b -> 2 * crazy2val(b)
      | ONE b -> 1 + 2 * crazy2val(b)
      | MONE b -> -1 + 2 * crazy2val(b)
  */
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        (a, b) match {
          case (NIL, _) => { b }
          case (_, NIL) => { a }
          case (ZERO(a_0), ZERO(b_0)) => { ZERO(crazy2add(a_0, b_0)) }
          case (ZERO(a_0), ONE(b_0)) => { ONE(crazy2add(a_0, b_0)) }
          case (ZERO(a_0), MONE(b_0)) => { MONE(crazy2add(a_0, b_0)) }
          case (ONE(a_0), ZERO(b_0)) => { ONE(crazy2add(a_0, b_0)) }
          case (ONE(a_0), ONE(b_0)) => {
            ZERO(crazy2add(a_0, crazy2add(b_0, ONE(NIL))))
          }
          case (ONE(a_0), MONE(b_0)) => { ZERO(crazy2add(a_0, b_0)) }
          case (MONE(a_0), ZERO(b_0)) => { MONE(crazy2add(a_0, b_0)) }
          case (MONE(a_0), ONE(b_0)) => { ZERO(crazy2add(a_0, b_0)) }
          case (MONE(a_0), MONE(b_0)) => {
            ZERO(crazy2add(a_0, crazy2add(b_0, MONE(NIL))))
          }
        }
    }
  }
  
  /* Test Code
  let x : crazy2 = ZERO(ONE(MONE NIL))
  let y : crazy2 = ONE(ZERO(ONE NIL))
  let z : crazy2 = ONE(MONE(ZERO(MONE NIL)))
  let w : crazy2 = ZERO(MONE NIL)
  let t : crazy2 = ONE(ONE(ONE(ONE(ONE NIL))))
  
  let test (a, b) = print_endline (string_of_int(crazy2val(crazy2add(a, b))))
  
  let _ = test (x, y)
  let _ = test (y, x)
  let _ = test (z, x)
  let _ = test (y, z)
  let _ = test (x, x)
  let _ = test (y, y)
  let _ = test (z, z)
  let _ = test (x, w)
  let _ = test (z, w)
  let _ = test (t, t)
  */
}