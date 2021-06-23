import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub38 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2val(crazyinput: Crazy2): Int63 = {
    val _2 = {
      def pow(a) = {
        (
          x =>
            x match {
              case 0 => { 1 }
              case 1 => { a }
              case n => {
                val _8 = {
                  val b = pow(a, n / 2)
                  b * b * (if (n % 2 == 0) 1 else a)
                }
              }
            }
        )
      }
      val _9 = {
        def subcrazy2val(c, i) = {
          c match {
            case NIL => { 0 }
            case ZERO(cc) => { subcrazy2val(cc, i + 1) }
            case ONE(cc) => { subcrazy2val(cc, i + 1) + pow(2, i) }
            case MONE(cc) => { subcrazy2val(cc, i + 1) - pow(2, i) }
          }
        }
        subcrazy2val(crazyinput, 0)
      }
    }
  }
  
  def crazy2add(((c1, c2))) = {
    val _12 = {
      def crazy2addsub(c1, c2, carry) = {
        (c1, c2, carry) match {
          case (NIL, _, 0) => { c2 }
          case (NIL, _, 1) => { crazy2addsub(ONE(NIL), c2, 0) }
          case (NIL, _, -1) => { crazy2addsub(MONE(NIL), c2, 0) }
          case (_, NIL, 0) => { c1 }
          case (_, NIL, 1) => { crazy2addsub(ONE(NIL), c1, 0) }
          case (_, NIL, -1) => { crazy2addsub(MONE(NIL), c1, 0) }
          case (ZERO(cc1), ZERO(cc2), 0) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), ZERO(cc2), 1) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), ZERO(cc2), -1) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), ONE(cc2), 0) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), ONE(cc2), 1) => { ZERO(crazy2addsub(cc1, cc2, 1)) }
          case (ZERO(cc1), ONE(cc2), -1) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), MONE(cc2), 0) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), MONE(cc2), 1) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (ZERO(cc1), MONE(cc2), -1) => {
            ZERO(crazy2addsub(cc1, cc2, -(1)))
          }
          case (ONE(cc1), ZERO(cc2), 0) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (ONE(cc1), ZERO(cc2), 1) => { ZERO(crazy2addsub(cc1, cc2, 1)) }
          case (ONE(cc1), ZERO(cc2), -1) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (ONE(cc1), ONE(cc2), 0) => { ZERO(crazy2addsub(cc1, cc2, 1)) }
          case (ONE(cc1), ONE(cc2), 1) => { ONE(crazy2addsub(cc1, cc2, 1)) }
          case (ONE(cc1), ONE(cc2), -1) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (ONE(cc1), MONE(cc2), 0) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (ONE(cc1), MONE(cc2), 1) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (ONE(cc1), MONE(cc2), -1) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), ZERO(cc2), 0) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), ZERO(cc2), 1) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), ZERO(cc2), -1) => {
            ZERO(crazy2addsub(cc1, cc2, -(1)))
          }
          case (MONE(cc1), ONE(cc2), 0) => { ZERO(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), ONE(cc2), 1) => { ONE(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), ONE(cc2), -1) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), MONE(cc2), 0) => { ZERO(crazy2addsub(cc1, cc2, -(1)))
          }
          case (MONE(cc1), MONE(cc2), 1) => { MONE(crazy2addsub(cc1, cc2, 0)) }
          case (MONE(cc1), MONE(cc2), -1) => {
            MONE(crazy2addsub(cc1, cc2, -(1)))
          }
          case _ => { NIL }
        }
      }
      crazy2addsub(c1, c2, 0)
    }
  }
  
  /* TEST CASE */
    /*
  let _ =
    print_int (crazy2val (ZERO (ONE (MONE NIL))));
    print_int (crazy2val (ONE (MONE NIL)));
    print_int (crazy2val (ONE (ZERO (ONE NIL))));
    print_endline "";
  ;;
  
  let _ =
    let z1 = (ZERO (ONE (MONE NIL))) in
    let z2 = (ONE (MONE NIL)) in
    let z3 = (ONE (ZERO (ONE NIL))) in
    let z4 = (ONE (ONE (ZERO (ZERO (MONE (ONE (MONE (ONE (ZERO (MONE NIL)))))))))) in
    let z5 = (ONE (MONE (ONE (ONE (MONE (MONE (ZERO (ONE (ONE (MONE (ONE NIL))))))))))) in
    print_int (crazy2val z3); print_endline "";
    print_int (crazy2val z4); print_endline "";
    print_int (crazy2val z5); print_endline "";
    print_int (crazy2val (crazy2add (z3, z4))); print_endline "";
    print_int (crazy2val (crazy2add (z5, z4))); print_endline "";
    print_int (crazy2val (crazy2add (z4, z5))); print_endline "";
  
    */
}