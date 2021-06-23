import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub94 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  sealed abstract class Help {}
  case object ZE extends Help {}
  case object ON extends Help {}
  case object MON extends Help {}
  def addhelp: (Crazy2, Crazy2, Help) => Crazy2 = {
    case (ca, cb, h) =>
      {
        (ca, cb, h) match {
          case (NIL, c0, _) | (c0, NIL, _) => {
            (c0, h) match {
              case (NIL, ZE) => { NIL }
              case (NIL, ON) => { ONE(NIL) }
              case (NIL, MON) => { MONE(NIL) }
              case (ZERO(c), ZE) | (ONE(c), MON) | (MONE(c), ON) => {
                ZERO(addhelp(NIL, c, ZE))
              }
              case (ZERO(c), ON) | (ONE(c), ZE) => { ONE(addhelp(NIL, c, ZE)) }
              case (ONE(c), ON) => { ZERO(addhelp(NIL, c, ON)) }
              case (ZERO(c), MON) | (MONE(c), ZE) => { MONE(addhelp(NIL, c, ZE))
              }
              case (MONE(c), MON) => { ZERO(addhelp(NIL, c, MON)) }
            }
          }
          case (ZERO(c1), ZERO(c2), ZE) | (ZERO(c1), ONE(c2), MON) |
          (ZERO(c1), MONE(c2), ON) | (ONE(c1), ZERO(c2), MON) |
          (ONE(c1), MONE(c2), ZE) | (MONE(c1), ZERO(c2), ON) |
          (MONE(c1), ONE(c2), ZE) => {
            ZERO(addhelp(c1, c2, ZE))
          }
          case (ZERO(c1), ZERO(c2), ON) | (ZERO(c1), ONE(c2), ZE) |
          (ONE(c1), ZERO(c2), ZE) | (ONE(c1), ONE(c2), MON) |
          (ONE(c1), MONE(c2), ON) | (MONE(c1), ONE(c2), ON) => {
            ONE(addhelp(c1, c2, ZE))
          }
          case (ONE(c1), ONE(c2), ZE) | (ONE(c1), ZERO(c2), ON) |
          (ZERO(c1), ONE(c2), ON) => {
            ZERO(addhelp(c1, c2, ON))
          }
          case (ONE(c1), ONE(c2), ON) => { ONE(addhelp(c1, c2, ON)) }
          case (ZERO(c1), ZERO(c2), MON) | (ZERO(c1), MONE(c2), ZE) |
          (MONE(c1), ZERO(c2), ZE) | (MONE(c1), MONE(c2), ON) |
          (MONE(c1), ONE(c2), MON) | (ONE(c1), MONE(c2), MON) => {
            MONE(addhelp(c1, c2, ZE))
          }
          case (MONE(c1), MONE(c2), ZE) | (MONE(c1), ZERO(c2), MON) |
          (ZERO(c1), MONE(c2), MON) => {
            ZERO(addhelp(c1, c2, MON))
          }
          case (MONE(c1), MONE(c2), MON) => { MONE(addhelp(c1, c2, MON)) }
        }
    }
  }
  
  
  val crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) => { addhelp(c1, c2, ZE) }
  }
  
  /*
  let rec helpeval : (crazy2*int*int) -> int = fun (c, ans, n) ->
          match c with
          | NIL -> ans
          | ZERO(c1) -> helpeval(c1,ans,n*2)
          | ONE(c1) -> helpeval(c1,ans+n,n*2)
          | MONE(c1) -> helpeval(c1,ans-n,n*2)
  let crazy2eval : crazy2 -> int = fun c -> helpeval(c,0,1)
  let a = ZERO(ONE(MONE(ZERO(MONE(ONE(ONE(NIL)))))))
  let b = MONE(MONE(MONE(ONE(ZERO(MONE(MONE(ONE(ZERO(NIL)))))))))
  let c = ONE(ZERO(ZERO(ONE(MONE(ONE(ONE(ZERO(MONE(ONE(NIL))))))))))
  let d = MONE(MONE(ONE(ZERO(ZERO(ONE(ONE(ONE(NIL))))))))
  let e = MONE(MONE(MONE(MONE(ONE(ONE(ONE(ONE(NIL))))))))
  let _ = print_endline(string_of_bool (crazy2eval(a)+crazy2eval(b)==crazy2eval(crazy2add(a,b))))
  let _ = print_endline(string_of_bool (crazy2eval(a)+crazy2eval(c)==crazy2eval(crazy2add(a,c))))
  let _ = print_endline(string_of_bool (crazy2eval(a)+crazy2eval(d)==crazy2eval(crazy2add(a,d))))
  let _ = print_endline(string_of_bool (crazy2eval(c)+crazy2eval(b)==crazy2eval(crazy2add(b,c))))
  let _ = print_endline(string_of_bool (crazy2eval(d)+crazy2eval(b)==crazy2eval(crazy2add(d,b))))
  let _ = print_endline(string_of_bool (crazy2eval(d)+crazy2eval(c)==crazy2eval(crazy2add(c,d))))
  let _ = print_endline(string_of_bool (crazy2eval(a)+crazy2eval(e)==crazy2eval(crazy2add(a,e))))
  let _ = print_endline(string_of_bool (crazy2eval(e)+crazy2eval(b)==crazy2eval(crazy2add(b,e))))
  let _ = print_endline(string_of_bool (crazy2eval(d)+crazy2eval(e)==crazy2eval(crazy2add(d,e))))
  let _ = print_endline(string_of_bool (crazy2eval(e)+crazy2eval(c)==crazy2eval(crazy2add(c,e))))
  */
}