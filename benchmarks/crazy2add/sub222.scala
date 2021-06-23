import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub222 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        (a, b) match {
          case (NIL, b) => { b }
          case (a, NIL) => { a }
          case (MONE(at), MONE(bt)) => {
            ZERO(crazy2add(crazy2add(MONE(NIL), at), bt))
          }
          case (MONE(at), ZERO(bt)) => { MONE(crazy2add(at, bt)) }
          case (MONE(at), ONE(bt)) => { ZERO(crazy2add(at, bt)) }
          case (ZERO(at), MONE(bt)) => { MONE(crazy2add(at, bt)) }
          case (ZERO(at), ZERO(bt)) => { ZERO(crazy2add(at, bt)) }
          case (ZERO(at), ONE(bt)) => { ONE(crazy2add(at, bt)) }
          case (ONE(at), MONE(bt)) => { ZERO(crazy2add(at, bt)) }
          case (ONE(at), ZERO(bt)) => { ONE(crazy2add(at, bt)) }
          case (ONE(at), ONE(bt)) => {
            ZERO(crazy2add(crazy2add(ONE(NIL), at), bt))
          }
        }
    }
  }
    
  
  /* TEST */
  /*
  let rec crazy2val: crazy2 -> int = fun(x) ->
    match x with
      |NIL -> 0
      |ZERO a -> crazy2val(a) * 2
      |ONE a -> crazy2val(a) * 2 + 1
      |MONE a -> crazy2val(a) * 2 - 1
  
  let mtwo = ZERO(ONE(MONE NIL)) 
  let one = ONE(NIL) 
  let five = ONE(ZERO(ONE NIL)) 
  let mone = ONE(MONE NIL) 
  let mnine = ONE(MONE(ZERO(MONE NIL))) 
  let zero = ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO NIL))))))) 
  let big1 = ZERO(ONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE(ONE NIL))))))))) 
  let big2 = ONE(MONE(MONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE NIL))))))))) 
  
  let _ = 
    let test_case : int * int * int -> unit = fun (n, x, y) -> 
      let result : int * int -> string = fun(x, y) -> 
        if(x == y) then "Pass" 
        else "Failure -> " ^ string_of_int(x) ^ " vs " ^ string_of_int(y) in 
      print_endline ("Case " ^ string_of_int(n) ^ " : " ^ result(x, y)) in 
    test_case (1, crazy2val(crazy2add(mnine, mtwo)), crazy2val(crazy2add(mtwo, mnine))); 
    test_case (2, crazy2val(mtwo) + crazy2val(mnine), crazy2val(crazy2add(mtwo, mnine))); 
    test_case (3, crazy2val(five) + crazy2val(mnine), crazy2val(crazy2add(five, mnine))); 
    test_case (4, crazy2val(crazy2add(mnine, one)) + crazy2val(five), crazy2val(mnine) + crazy2val(crazy2add(one, five))); 
    test_case (5, crazy2val(crazy2add(crazy2add(mnine, mtwo), crazy2add(five, mtwo))), crazy2val(mnine) + crazy2val(mtwo) + crazy2val(five) + crazy2val(mtwo)); 
    test_case (6, crazy2val(crazy2add(zero, mnine)), crazy2val(mnine)); 
    test_case (7, crazy2val(crazy2add(big1, big2)), crazy2val(crazy2add(big2, big1))); 
    test_case (8, crazy2val(crazy2add(big1, crazy2add(big1, big2))), crazy2val(crazy2add(crazy2add(big1, big1), big2)));
    */
}