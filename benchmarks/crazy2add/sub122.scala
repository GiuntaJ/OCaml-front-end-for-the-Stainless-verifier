import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub122 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((num1, num2))) = {
    (num1, num2) match {
      case (ZERO(x), ZERO(y)) => { ZERO(crazy2add(x, y)) }
      case (ZERO(x), ONE(y)) | (ONE(x), ZERO(y)) => { ONE(crazy2add(x, y)) }
      case (MONE(x), ZERO(y)) | (ZERO(x), MONE(y)) => { MONE(crazy2add(x, y)) }
      case (MONE(x), ONE(y)) | (ONE(x), MONE(y)) => { ZERO(crazy2add(x, y)) }
      case (ONE(x), ONE(y)) => { ZERO(crazy2add(ONE(NIL), crazy2add(x, y))) }
      case (MONE(x), MONE(y)) => { ZERO(crazy2add(MONE(NIL), crazy2add(x, y))) }
      case (something, NIL) | (NIL, something) => { something }
    }
  }
  
  
  
  /* let mtwo = ZERO(ONE(MONE NIL))
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
    test_case (8, crazy2val(crazy2add(big1, crazy2add(big1, big2))), crazy2val(crazy2add(crazy2add(big1, big1), big2))); */
}