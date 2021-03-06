import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub168 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        (c1, c2) match {
          case (NIL, NIL) => { NIL }
          case (ONE(c1), NIL) => { ONE(crazy2add(c1, NIL)) }
          case (ZERO(c1), NIL) => { ZERO(crazy2add(c1, NIL)) }
          case (MONE(c1), NIL) => { MONE(crazy2add(c1, NIL)) }
          case (NIL, ONE(c2)) => { ONE(crazy2add(NIL, c2)) }
          case (NIL, ZERO(c2)) => { ZERO(crazy2add(NIL, c2)) }
          case (NIL, MONE(c2)) => { MONE(crazy2add(NIL, c2)) }
          case (ONE(c1), ONE(c2)) => {
            ZERO(crazy2add(crazy2add(c1, ONE(NIL)), c2))
          }
          case (ONE(c1), ZERO(c2)) => { ONE(crazy2add(c1, c2)) }
          case (ZERO(c1), ONE(c2)) => { ONE(crazy2add(c1, c2)) }
          case (ZERO(c1), ZERO(c2)) => { ZERO(crazy2add(c1, c2)) }
          case (ONE(c1), MONE(c2)) => { ZERO(crazy2add(c1, c2)) }
          case (MONE(c1), MONE(c2)) => {
            ZERO(crazy2add(crazy2add(c1, MONE(NIL)), c2))
          }
          case (ZERO(c1), MONE(c2)) => { MONE(crazy2add(c1, c2)) }
          case (MONE(c1), ZERO(c2)) => { MONE(crazy2add(c1, c2)) }
          case (MONE(c1), ONE(c2)) => { ZERO(crazy2add(c1, c2)) }
        }
    }
  }
  
  /*        
          let rec crazy2val:crazy2->int = fun crazy2 ->
                  match crazy2 with
                      |NIL -> 0
                          |ZERO crazy -> 0 + 2 * crazy2val crazy
                              |ONE crazy -> 1 + 2 * crazy2val crazy
                                  |MONE crazy -> -1 + 2 * crazy2val crazy
  
  
          let mtwo = ZERO(ONE(MONE NIL)) 
          let one = ONE(NIL) 
          let five = ONE(ZERO(ONE NIL)) 
          let mone = ONE(MONE NIL) 
          let mnine = ONE(MONE(ZERO(MONE NIL))) 
          let zero = ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO NIL))))))) 
          let five2 = ONE(ZERO(ONE(ZERO(ZERO(ZERO(ZERO(ZERO NIL))))))) 
          let big1 = ZERO(ONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE(ONE NIL))))))))) 
          let big2 = ONE(MONE(MONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE NIL))))))))) 
  
          let _ = 
                let test_case : int * int * int -> unit = fun (n, x, y) -> 
                        let result : int * int -> string = fun(x, y) -> 
                                  if(x == y) then "Pass" 
                                        else "Failure -> " ^ string_of_int(x) ^ " vs " ^ string_of_int(y) in 
                            print_endline ("Case " ^ string_of_int(n) ^ " : " ^ result(x, y)) in 
                  let test_nocrazy2val : bool -> unit = fun x -> 
                          let detector = fun x -> if(x = true) then "Pass" else "crazy2val detected" in 
                              print_endline ("crazy2val detector : " ^ detector(x)) in 
                    test_nocrazy2val (crazy2add(zero, five) = five2); 
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