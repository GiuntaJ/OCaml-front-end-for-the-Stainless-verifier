import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub99 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  /*
  let rec crazy2val = function
  	| NIL -> 0
  	| ZERO NIL -> 0
  	| ONE NIL -> 1
  	| MONE NIL -> -1
  	| ZERO c -> 2*(crazy2val c)
  	| ONE c -> 1+2*(crazy2val c)
  	| MONE c -> -1+2*(crazy2val c)
  */
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = (
    x =>
      x match {
        case (c, NIL) | (NIL, c) => { c }
        case (ZERO(c1), ZERO(c2)) | (ONE(c1), MONE(c2)) | (MONE(c1), ONE(c2)) => {
          ZERO(crazy2add(c1, c2))
        }
        case (ZERO(c1), ONE(c2)) | (ONE(c1), ZERO(c2)) => {
          ONE(crazy2add(c1, c2))
        }
        case (ZERO(c1), MONE(c2)) | (MONE(c1), ZERO(c2)) => {
          MONE(crazy2add(c1, c2))
        }
        case (ONE(c1), ONE(c2)) => {
          ZERO(crazy2add(crazy2add(ONE(NIL), c1), c2))
        }
        case (MONE(c1), MONE(c2)) => {
          ZERO(crazy2add(crazy2add(MONE(NIL), c1), c2))
        }
      }
  )
  
  /*
  let _= let print_bool x = print_endline (string_of_bool x) in
  print_bool ( 0=(crazy2val (crazy2add (ZERO NIL, ZERO NIL)) ) );
  print_bool ( 0=(crazy2val (crazy2add (MONE NIL, ONE NIL))));
  print_bool ( 1=(crazy2val (crazy2add (ZERO NIL, ONE NIL))));
  print_bool ( 4=(crazy2val (crazy2add (ONE(ONE NIL), ONE NIL))));
  print_bool ( -683 = (crazy2val (crazy2add ( MONE(ZERO(ZERO(ZERO NIL))), (ZERO(ONE(ONE(ZERO(MONE(ONE(ONE(ZERO(ONE(ZERO(MONE NIL)))))))))))))));;
  */
}