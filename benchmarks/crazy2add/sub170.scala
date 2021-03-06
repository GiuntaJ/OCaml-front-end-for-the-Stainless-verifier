import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub170 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        (c1, c2) match {
          case (NIL, _) => { c2 }
          case (_, NIL) => { c1 }
          case (ZERO(s1), ZERO(s2)) => { ZERO(crazy2add(s1, s2)) }
          case (ZERO(s1), ONE(s2)) => { ONE(crazy2add(s1, s2)) }
          case (ZERO(s1), MONE(s2)) => { MONE(crazy2add(s1, s2)) }
          case (ONE(s1), ZERO(s2)) => { ONE(crazy2add(s1, s2)) }
          case (ONE(s1), ONE(s2)) => {
            ZERO(crazy2add(ONE(NIL), crazy2add(s1, s2)))
          }
          case (ONE(s1), MONE(s2)) => { ZERO(crazy2add(s1, s2)) }
          case (MONE(s1), ZERO(s2)) => { MONE(crazy2add(s1, s2)) }
          case (MONE(s1), ONE(s2)) => { ZERO(crazy2add(s1, s2)) }
          case (MONE(s1), MONE(s2)) => {
            ZERO(crazy2add(MONE(NIL), crazy2add(s1, s2)))
          }
        }
    }
  }
  
  
  /*
  let rec crazy2val : crazy2 -> int = fun c ->
  	match c with
  	| NIL -> 0
  	| ZERO s -> 2*crazy2val(s)
  	| ONE s -> 1+2*crazy2val(s)
  	| MONE s -> -1+2*crazy2val(s)
  
  
  
  
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